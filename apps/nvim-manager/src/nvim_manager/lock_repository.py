"""Lockfile repository operations."""

import shutil
import socket
from pathlib import Path

from git import Repo

from .config import LockRepositoryConfig
from .logging import get_logger

logger = get_logger(__name__)


class LockRepository:
    """Manages the lock repository for storing plugin and editor locks.

    Attributes
    ----------
    config : LockRepositoryConfig
        Configuration for repository URLs and local path.
    repo : Repo
        GitPython repository instance, initialized on construction.

    """

    def __init__(self, config: LockRepositoryConfig) -> None:
        """Initialize lock repository manager.

        Ensures repository is cloned and configured upon instantiation.

        Parameters
        ----------
        config : LockRepositoryConfig
            Configuration for repository URLs and local path.

        Raises
        ------
        git.exc.GitCommandError
            If cloning fails or remote operations fail.
        git.exc.InvalidGitRepositoryError
            If existing directory is not a valid Git repository.
        OSError
            If filesystem operations fail (directory creation, permissions).

        """
        self.config = config
        logger.debug("Initializing LockRepository with path: %s", config.path)

        if not self.config.path.exists():
            logger.info(
                "Cloning lockfile repository from %s to %s",
                self.config.https_uri,
                self.config.path,
            )
            self.config.path.parent.mkdir(parents=True, exist_ok=True)
            Repo.clone_from(self.config.https_uri, self.config.path)
            logger.info("Repository cloned successfully")
        else:
            logger.debug("Repository already exists locally")

        self.repo = Repo(self.config.path)

        origin = self.repo.remotes.origin

        if origin.url != self.config.https_uri:
            logger.debug(
                "Updating fetch URL: %s -> %s",
                origin.url,
                self.config.https_uri,
            )
            origin.set_url(self.config.https_uri)
        else:
            logger.debug("Fetch URL already correct: %s", origin.url)

        try:
            push_url = self.repo.git.remote("get-url", "--push", "origin")
            if push_url != self.config.ssh_uri:
                logger.debug(
                    "Updating push URL: %s -> %s",
                    push_url,
                    self.config.ssh_uri,
                )
                origin.set_url(self.config.ssh_uri, push=True)
            else:
                logger.debug("Push URL already correct: %s", self.config.ssh_uri)
        except (AttributeError, ValueError) as e:
            logger.debug(
                "Could not determine current push URL (%s), updating to: %s",
                e,
                self.config.ssh_uri,
            )
            origin.set_url(self.config.ssh_uri, push=True)

    def sync_from_remote(self) -> None:
        """Sync local repository with remote state.

        Performs hard reset to match remote main branch.

        Raises
        ------
        git.exc.GitCommandError
            If git operations fail.

        """
        logger.debug("Syncing from remote repository")
        self.repo.remotes.origin.fetch()
        try:
            ahead_count = len(list(self.repo.iter_commits("HEAD..origin/main")))
            behind_count = len(list(self.repo.iter_commits("origin/main..HEAD")))
            if ahead_count > 0 and behind_count > 0:
                logger.warning(
                    "Local repository diverged: %d ahead, %d behind; changes will be lost!",
                    behind_count,
                    ahead_count,
                )
            elif ahead_count > 0:
                logger.debug("Remote has %d new commits", ahead_count)
            elif behind_count > 0:
                logger.warning(
                    "Local has %d unpushed commits; changes will be lost!",
                    behind_count,
                )
            else:
                logger.debug("Local and remote are in sync")
        except (AttributeError, ValueError) as e:
            logger.debug("Could not determine ahead/behind status: %s", e)

        self.repo.head.reset("origin/main", index=True, working_tree=True)
        logger.debug("Repository synced successfully")

    def commit_and_push(self, file_path: str) -> None:
        """Commit single file and push to remote repository if needed.

        Only commits if the file has changes and only pushes if local commits are ahead
        of remote.

        Parameters
        ----------
        file_path : str
            Repository-relative path of file to commit.

        Raises
        ------
        git.exc.GitCommandError
            If git operations fail.

        """
        logger.debug("Committing changes in %s", file_path)
        self.repo.git.add(file_path)
        if self.repo.is_dirty(untracked_files=True):
            hostname = socket.gethostname().split(".")[0]
            self.repo.index.commit(f"Update {file_path}\n\nHost: {hostname}")
            logger.info("Committed changes in %s", file_path)
        else:
            logger.debug("No changes to commit in %s", file_path)

        logger.debug("Pushing changes to remote repository")
        origin = self.repo.remotes.origin
        try:
            local_commit = self.repo.head.commit
            remote_commit = origin.refs.main.commit
            if local_commit != remote_commit:
                origin.push("main", force=True)
                logger.info("Changes pushed to remote repository")
            else:
                logger.debug("No changes to push to remote repository")
        except (AttributeError, ValueError) as e:
            # If we can't determine the state, push anyway
            logger.debug("Could not compare commits, pushing anyway: %s", e)
            origin.push("main", force=True)
            logger.info("Changes pushed to remote repository")

    def read_file(self, repo_relative_path: str) -> str:
        """Read file content from repository with auto-sync.

        Parameters
        ----------
        repo_relative_path : str
            Relative path within repository of file to read.

        Returns
        -------
        str
            Content of the file.

        Raises
        ------
        FileNotFoundError
            If repository file doesn't exist.

        """
        logger.debug("Reading file: %s", repo_relative_path)
        self.sync_from_remote()

        file_path = self.config.path / repo_relative_path
        if not file_path.exists():
            msg = f"Repository file not found: {repo_relative_path}"
            logger.error(msg)
            raise FileNotFoundError(msg)

        content = file_path.read_text(encoding="utf-8")
        logger.debug(
            "Successfully read file: %s (%d chars)",
            repo_relative_path,
            len(content),
        )
        return content

    def write_file(self, repo_relative_path: str, content: str) -> None:
        """Write content to repository file with auto-commit and push.

        Parameters
        ----------
        repo_relative_path : str
            Relative path within repository where file should be written.
        content : str
            Content to write to the file.

        """
        logger.debug("Writing file: %s (%d chars)", repo_relative_path, len(content))
        file_path = self.config.path / repo_relative_path
        file_path.parent.mkdir(parents=True, exist_ok=True)

        file_path.write_text(content, encoding="utf-8")
        logger.debug("File written locally: %s", repo_relative_path)

        self.commit_and_push(repo_relative_path)

    def get_file(self, repo_relative_path: str, dest_path: Path) -> None:
        """Get file from repository to local filesystem with auto-sync.

        Parameters
        ----------
        repo_relative_path : str
            Relative path within repository of source file.
        dest_path : Path
            Local destination path for the file.

        Raises
        ------
        FileNotFoundError
            If repository file doesn't exist.

        """
        logger.debug(
            "Getting file from repository: %s -> %s",
            repo_relative_path,
            dest_path,
        )
        self.sync_from_remote()

        source_path = self.config.path / repo_relative_path
        if not source_path.exists():
            msg = f"Repository file not found: {repo_relative_path}"
            logger.error(msg)
            raise FileNotFoundError(msg)

        dest_path.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source_path, dest_path)
        logger.info(
            "File copied from repository: %s -> %s",
            repo_relative_path,
            dest_path,
        )

    def put_file(self, source_path: Path, repo_relative_path: str) -> None:
        """Put local file into repository with auto-commit and push.

        Parameters
        ----------
        source_path : Path
            Path to source file to copy.
        repo_relative_path : str
            Relative path within repository where file should be placed.

        Raises
        ------
        FileNotFoundError
            If source file doesn't exist.

        """
        logger.debug(
            "Putting file into repository: %s -> %s",
            source_path,
            repo_relative_path,
        )
        if not source_path.exists():
            msg = f"Source file not found: {source_path}"
            logger.error(msg)
            raise FileNotFoundError(msg)

        dest_path = self.config.path / repo_relative_path
        dest_path.parent.mkdir(parents=True, exist_ok=True)

        shutil.copy2(source_path, dest_path)
        logger.debug(
            "File copied to repository: %s -> %s",
            source_path,
            repo_relative_path,
        )

        self.commit_and_push(repo_relative_path)

    def has_file(self, repo_relative_path: str) -> bool:
        """Check if file exists in the lock repository with auto-sync.

        Parameters
        ----------
        repo_relative_path : str
            Relative path within repository to check.

        Returns
        -------
        bool
            True if file exists in repository, False otherwise.

        """
        logger.debug("Checking if file exists: %s", repo_relative_path)
        if not self.config.path.exists():
            logger.debug("Repository path does not exist")
            return False

        self.sync_from_remote()
        exists = (self.config.path / repo_relative_path).exists()
        logger.debug("File exists check for %s: %s", repo_relative_path, exists)
        return exists
