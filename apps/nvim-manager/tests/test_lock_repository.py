"""Tests for LockRepository class."""

from unittest.mock import MagicMock, patch

import pytest
from git.exc import GitCommandError, InvalidGitRepositoryError

from nvim_manager.config import LockRepositoryConfig
from nvim_manager.lock_repository import LockRepository


@pytest.fixture
def mock_config(tmp_path):
    """Create a test configuration with temporary paths."""
    return LockRepositoryConfig(
        https_uri="https://github.com/test/repo.git",
        ssh_uri="git@github.com:test/repo.git",
        path=tmp_path / "lock-repo",
    )


class TestLockRepositoryConstructor:
    """Test LockRepository constructor behavior."""

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_with_existing_repo_urls_differ(
        self,
        mock_repo_class,
        mock_config,
    ):
        """Test constructor when repository exists but URLs need updating."""
        # Setup: create directory and mock existing repo with different URLs
        mock_config.path.mkdir(parents=True)
        mock_repo_instance = MagicMock()
        mock_origin = MagicMock()
        mock_origin.url = "https://old-url.git"  # Different from config
        mock_origin.urls = [
            "https://old-url.git",
            "git@old-url.git",
        ]  # Different push URL
        mock_repo_instance.remotes.origin = mock_origin
        mock_repo_class.return_value = mock_repo_instance

        # Test: create LockRepository
        lock_repo = LockRepository(mock_config)

        # Verify: no cloning, repo loading and URLs updated
        mock_repo_class.clone_from.assert_not_called()
        mock_repo_class.assert_called_once_with(mock_config.path)
        mock_origin.set_url.assert_any_call(mock_config.https_uri)
        mock_origin.set_url.assert_any_call(mock_config.ssh_uri, push=True)

        assert lock_repo.config == mock_config
        assert lock_repo.repo == mock_repo_instance

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_with_existing_repo_urls_same(
        self,
        mock_repo_class,
        mock_config,
    ):
        """Test constructor when repository exists and URLs are already correct."""
        # Setup: create directory and mock existing repo with same URLs
        mock_config.path.mkdir(parents=True)
        mock_repo_instance = MagicMock()
        mock_origin = MagicMock()
        mock_origin.url = mock_config.https_uri  # Same as config
        mock_origin.urls = [mock_config.https_uri, mock_config.ssh_uri]  # Same push URL
        mock_repo_instance.remotes.origin = mock_origin
        mock_repo_class.return_value = mock_repo_instance

        # Test: create LockRepository
        lock_repo = LockRepository(mock_config)

        # Verify: no cloning, repo loading but URLs not updated
        mock_repo_class.clone_from.assert_not_called()
        mock_repo_class.assert_called_once_with(mock_config.path)
        mock_origin.set_url.assert_not_called()

        assert lock_repo.config == mock_config
        assert lock_repo.repo == mock_repo_instance

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_clones_missing_repo(
        self,
        mock_repo_class,
        mock_config,
    ):
        """Test constructor clones repository when it doesn't exist."""
        # Setup: mock repo instance and cloning
        mock_repo_instance = MagicMock()
        mock_origin = MagicMock()
        mock_origin.url = (
            "https://old-url.git"  # Different from config to trigger update
        )
        mock_origin.urls = ["https://old-url.git", "git@old-url.git"]  # Different URLs
        mock_repo_instance.remotes.origin = mock_origin
        mock_repo_class.return_value = mock_repo_instance

        # Test: create LockRepository (path doesn't exist)
        LockRepository(mock_config)

        # Verify: cloning occurred and parent directories created, URLs updated
        assert mock_config.path.parent.exists()
        mock_repo_class.clone_from.assert_called_once_with(
            mock_config.https_uri,
            mock_config.path,
        )
        mock_repo_class.assert_called_with(mock_config.path)
        mock_origin.set_url.assert_any_call(mock_config.https_uri)
        mock_origin.set_url.assert_any_call(mock_config.ssh_uri, push=True)

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_handles_clone_failure(self, mock_repo_class, mock_config):
        """Test constructor raises GitCommandError when cloning fails."""
        # Setup: mock clone failure
        mock_repo_class.clone_from.side_effect = GitCommandError(
            "clone",
            "Failed to clone repository",
        )

        # Test: constructor should raise GitCommandError
        with pytest.raises(GitCommandError):
            LockRepository(mock_config)

        # Verify: clone was attempted
        mock_repo_class.clone_from.assert_called_once()

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_handles_invalid_git_repo(self, mock_repo_class, mock_config):
        """Test constructor raises InvalidGitRepositoryError for invalid repos."""
        # Setup: directory exists but isn't a Git repo
        mock_config.path.mkdir(parents=True)
        mock_repo_class.side_effect = InvalidGitRepositoryError("Not a Git repo")

        # Test: constructor should raise InvalidGitRepositoryError
        with pytest.raises(InvalidGitRepositoryError):
            LockRepository(mock_config)

        # Verify: no cloning attempted, Repo() was called
        mock_repo_class.clone_from.assert_not_called()
        mock_repo_class.assert_called_once_with(mock_config.path)

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_handles_remote_setup_failure(
        self,
        mock_repo_class,
        mock_config,
    ):
        """Test constructor raises GitCommandError when remote setup fails."""
        # Setup: repo exists but remote setup fails
        mock_config.path.mkdir(parents=True)
        mock_repo_instance = MagicMock()
        mock_origin = MagicMock()
        mock_origin.set_url.side_effect = GitCommandError("remote", "Failed to set URL")
        mock_repo_instance.remotes.origin = mock_origin
        mock_repo_class.return_value = mock_repo_instance

        # Test: constructor should raise GitCommandError
        with pytest.raises(GitCommandError):
            LockRepository(mock_config)

    def test_constructor_handles_filesystem_error(self, mock_config):
        """Test constructor raises OSError for filesystem issues."""
        # Setup: make parent directory read-only to cause mkdir failure
        mock_config.path.parent.mkdir(parents=True, exist_ok=True)
        mock_config.path.parent.chmod(0o444)  # Read-only

        try:
            # Test: constructor should raise OSError/PermissionError
            with pytest.raises((OSError, PermissionError)):
                LockRepository(mock_config)
        finally:
            # Cleanup: restore permissions
            mock_config.path.parent.chmod(0o755)

    @patch("nvim_manager.lock_repository.Repo")
    def test_constructor_creates_parent_directories(
        self,
        mock_repo_class,
        tmp_path,
    ):
        """Test constructor creates parent directories when needed."""
        # Setup: use nested path that doesn't exist
        nested_path = tmp_path / "deep" / "nested" / "repo"
        mock_config = LockRepositoryConfig(
            https_uri="https://github.com/test/repo.git",
            ssh_uri="git@github.com:test/repo.git",
            path=nested_path,
        )

        mock_repo_instance = MagicMock()
        mock_origin = MagicMock()
        mock_repo_instance.remotes.origin = mock_origin
        mock_repo_class.return_value = mock_repo_instance

        # Test: create LockRepository
        LockRepository(mock_config)

        # Verify: parent directories were created
        assert nested_path.parent.exists()
        assert nested_path.parent.is_dir()


class TestLockRepositoryFileOperations:
    """Test LockRepository file operation methods."""

    @pytest.fixture
    def lock_repo(self, mock_config):
        """Create a LockRepository instance with mocked repo."""
        with patch("nvim_manager.lock_repository.Repo") as mock_repo_class:
            mock_config.path.mkdir(parents=True)
            mock_repo_instance = MagicMock()
            mock_origin = MagicMock()
            mock_repo_instance.remotes.origin = mock_origin
            mock_repo_class.return_value = mock_repo_instance

            repo = LockRepository(mock_config)
            # Store mocks for test access
            repo._mock_repo = mock_repo_instance
            repo._mock_origin = mock_origin
            return repo

    def test_read_file_success(self, lock_repo, mock_config):
        """Test successful file reading with auto-sync."""
        # Setup: create test file
        test_file = mock_config.path / "test.txt"
        test_content = "test content"
        test_file.write_text(test_content)

        # Test: read file
        result = lock_repo.read_file("test.txt")

        # Verify: content matches and sync was called
        assert result == test_content
        lock_repo._mock_origin.fetch.assert_called_once()

    def test_read_file_not_found(self, lock_repo):
        """Test reading non-existent file raises FileNotFoundError."""
        with pytest.raises(FileNotFoundError, match="Repository file not found"):
            lock_repo.read_file("nonexistent.txt")

    def test_write_file_success(self, lock_repo, mock_config):
        """Test successful file writing with auto-commit."""
        # Setup: mock repo state to show changes exist
        lock_repo._mock_repo.is_dirty.return_value = True

        # Mock commit objects for push comparison
        mock_local_commit = MagicMock()
        mock_remote_commit = MagicMock()
        mock_local_commit.__ne__ = MagicMock(return_value=True)  # Different commits
        lock_repo._mock_repo.head.commit = mock_local_commit
        lock_repo._mock_origin.refs.main.commit = mock_remote_commit

        # Test: write file
        test_content = "new content"
        lock_repo.write_file("new.txt", test_content)

        # Verify: file was written and commit was called with specific file
        written_file = mock_config.path / "new.txt"
        assert written_file.exists()
        assert written_file.read_text() == test_content
        lock_repo._mock_repo.git.add.assert_called_with("new.txt")
        lock_repo._mock_origin.push.assert_called_once()

    def test_get_file_success(self, lock_repo, mock_config, tmp_path):
        """Test successful file retrieval with auto-sync."""
        # Setup: create source file in repo
        repo_file = mock_config.path / "source.txt"
        test_content = "source content"
        repo_file.write_text(test_content)

        # Test: get file
        dest_file = tmp_path / "dest.txt"
        lock_repo.get_file("source.txt", dest_file)

        # Verify: file copied and sync was called
        assert dest_file.exists()
        assert dest_file.read_text() == test_content
        lock_repo._mock_origin.fetch.assert_called_once()

    def test_get_file_not_found(self, lock_repo, tmp_path):
        """Test getting non-existent file raises FileNotFoundError."""
        dest_file = tmp_path / "dest.txt"
        with pytest.raises(FileNotFoundError, match="Repository file not found"):
            lock_repo.get_file("nonexistent.txt", dest_file)

    def test_put_file_success(self, lock_repo, mock_config, tmp_path):
        """Test successful file putting with auto-commit."""
        # Setup: create source file
        source_file = tmp_path / "source.txt"
        test_content = "put content"
        source_file.write_text(test_content)

        # Setup: mock repo state to show changes exist
        lock_repo._mock_repo.is_dirty.return_value = True

        # Mock commit objects for push comparison
        mock_local_commit = MagicMock()
        mock_remote_commit = MagicMock()
        mock_local_commit.__ne__ = MagicMock(return_value=True)  # Different commits
        lock_repo._mock_repo.head.commit = mock_local_commit
        lock_repo._mock_origin.refs.main.commit = mock_remote_commit

        # Test: put file
        lock_repo.put_file(source_file, "target.txt")

        # Verify: file copied to repo and commit was called with specific file
        repo_file = mock_config.path / "target.txt"
        assert repo_file.exists()
        assert repo_file.read_text() == test_content
        lock_repo._mock_repo.git.add.assert_called_with("target.txt")
        lock_repo._mock_origin.push.assert_called_once()

    def test_put_file_source_not_found(self, lock_repo, tmp_path):
        """Test putting non-existent source file raises FileNotFoundError."""
        source_file = tmp_path / "nonexistent.txt"
        with pytest.raises(FileNotFoundError, match="Source file not found"):
            lock_repo.put_file(source_file, "target.txt")

    def test_has_file_exists(self, lock_repo, mock_config):
        """Test has_file returns True for existing file with auto-sync."""
        # Setup: create test file
        test_file = mock_config.path / "exists.txt"
        test_file.write_text("content")

        # Test: check file existence
        result = lock_repo.has_file("exists.txt")

        # Verify: returns True and sync was called
        assert result is True
        lock_repo._mock_origin.fetch.assert_called_once()

    def test_has_file_not_exists(self, lock_repo):
        """Test has_file returns False for non-existent file."""
        result = lock_repo.has_file("nonexistent.txt")
        assert result is False

    def test_has_file_repo_not_exists(self, mock_config):
        """Test has_file returns False when repository doesn't exist."""
        # Don't create the repository path
        repo = LockRepository.__new__(LockRepository)
        repo.config = mock_config

        result = repo.has_file("any.txt")
        assert result is False


class TestLockRepositoryCommitAndPush:
    """Test commit_and_push smart behavior."""

    @pytest.fixture
    def lock_repo(self, mock_config):
        """Create a LockRepository instance with mocked repo."""
        with patch("nvim_manager.lock_repository.Repo") as mock_repo_class:
            mock_config.path.mkdir(parents=True)
            mock_repo_instance = MagicMock()
            mock_origin = MagicMock()
            mock_repo_instance.remotes.origin = mock_origin
            mock_repo_class.return_value = mock_repo_instance

            repo = LockRepository(mock_config)
            # Store mocks for test access
            repo._mock_repo = mock_repo_instance
            repo._mock_origin = mock_origin
            return repo

    def test_commit_and_push_with_changes_and_push_needed(self, lock_repo):
        """Test commit and push when both are needed."""
        # Setup: mock repo state with changes and different commits
        lock_repo._mock_repo.is_dirty.return_value = True
        mock_local_commit = MagicMock()
        mock_remote_commit = MagicMock()
        mock_local_commit.__ne__ = MagicMock(return_value=True)  # Different commits
        lock_repo._mock_repo.head.commit = mock_local_commit
        lock_repo._mock_origin.refs.main.commit = mock_remote_commit

        # Test: commit and push
        lock_repo.commit_and_push("test.txt")

        # Verify: both commit and push occurred
        lock_repo._mock_repo.git.add.assert_called_with("test.txt")
        lock_repo._mock_repo.index.commit.assert_called_once()
        lock_repo._mock_origin.push.assert_called_once()

    def test_commit_and_push_no_changes_no_push_needed(self, lock_repo):
        """Test commit and push when neither are needed."""
        # Setup: mock repo state with no changes and same commits
        lock_repo._mock_repo.is_dirty.return_value = False
        mock_commit = MagicMock()
        lock_repo._mock_repo.head.commit = mock_commit
        lock_repo._mock_origin.refs.main.commit = mock_commit

        # Test: commit and push
        lock_repo.commit_and_push("test.txt")

        # Verify: no commit or push occurred
        lock_repo._mock_repo.git.add.assert_called_with("test.txt")
        lock_repo._mock_repo.index.commit.assert_not_called()
        lock_repo._mock_origin.push.assert_not_called()

    def test_commit_and_push_no_changes_but_push_needed(self, lock_repo):
        """Test commit and push when only push is needed."""
        # Setup: mock repo state with no changes but different commits
        lock_repo._mock_repo.is_dirty.return_value = False
        mock_local_commit = MagicMock()
        mock_remote_commit = MagicMock()
        mock_local_commit.__ne__ = MagicMock(return_value=True)  # Different commits
        lock_repo._mock_repo.head.commit = mock_local_commit
        lock_repo._mock_origin.refs.main.commit = mock_remote_commit

        # Test: commit and push
        lock_repo.commit_and_push("test.txt")

        # Verify: no commit but push occurred
        lock_repo._mock_repo.git.add.assert_called_with("test.txt")
        lock_repo._mock_repo.index.commit.assert_not_called()
        lock_repo._mock_origin.push.assert_called_once()

    def test_commit_and_push_with_changes_no_push_needed(self, lock_repo):
        """Test commit and push when only commit is needed."""
        # Setup: mock repo state with changes but same commits
        lock_repo._mock_repo.is_dirty.return_value = True
        mock_commit = MagicMock()
        lock_repo._mock_repo.head.commit = mock_commit
        lock_repo._mock_origin.refs.main.commit = mock_commit

        # Test: commit and push
        lock_repo.commit_and_push("test.txt")

        # Verify: commit occurred but no push
        lock_repo._mock_repo.git.add.assert_called_with("test.txt")
        lock_repo._mock_repo.index.commit.assert_called_once()
        lock_repo._mock_origin.push.assert_not_called()

    def test_commit_and_push_comparison_fails_fallback_push(self, lock_repo):
        """Test commit and push when commit comparison fails, falls back to push."""
        # Setup: mock repo state with changes and comparison failure
        lock_repo._mock_repo.is_dirty.return_value = True
        lock_repo._mock_repo.head.commit = MagicMock()
        # Make refs.main.commit raise AttributeError
        lock_repo._mock_origin.refs.main.commit = MagicMock(
            side_effect=AttributeError("No refs")
        )

        # Test: commit and push
        lock_repo.commit_and_push("test.txt")

        # Verify: commit occurred and push fell back due to comparison failure
        lock_repo._mock_repo.git.add.assert_called_with("test.txt")
        lock_repo._mock_repo.index.commit.assert_called_once()
        lock_repo._mock_origin.push.assert_called_once()

    def test_commit_and_push_uses_hostname_in_commit_message(self, lock_repo):
        """Test that commit message includes hostname."""
        # Setup: mock repo state with changes
        lock_repo._mock_repo.is_dirty.return_value = True
        mock_local_commit = MagicMock()
        mock_remote_commit = MagicMock()
        mock_local_commit.__ne__ = MagicMock(return_value=True)
        lock_repo._mock_repo.head.commit = mock_local_commit
        lock_repo._mock_origin.refs.main.commit = mock_remote_commit

        # Test: commit and push
        with patch("socket.gethostname", return_value="test-host.example.com"):
            lock_repo.commit_and_push("test.txt")

        # Verify: commit message contains hostname
        expected_message = "Update test.txt\n\nHost: test-host"
        lock_repo._mock_repo.index.commit.assert_called_with(expected_message)
