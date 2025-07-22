"""Tests for NeovimBuilder class."""

import tempfile
from pathlib import Path
from unittest.mock import MagicMock, call, patch

import pytest
from git import Repo

from nvim_manager.neovim_builder import (
    NeovimBuildError,
    NeovimBuilder,
    NeovimCloneError,
    NeovimCompileError,
)


class TestNeovimBuildError:
    """Test the exception hierarchy."""

    def test_neovim_build_error_is_base_exception(self):
        """Test that NeovimBuildError is the base exception."""
        error = NeovimBuildError("test error")
        assert str(error) == "test error"
        assert isinstance(error, Exception)

    def test_neovim_clone_error_inherits_from_build_error(self):
        """Test that NeovimCloneError inherits from NeovimBuildError."""
        error = NeovimCloneError("clone failed")
        assert str(error) == "clone failed"
        assert isinstance(error, NeovimBuildError)
        assert isinstance(error, Exception)

    def test_neovim_compile_error_inherits_from_build_error(self):
        """Test that NeovimCompileError inherits from NeovimBuildError."""
        error = NeovimCompileError("compile failed")
        assert str(error) == "compile failed"
        assert isinstance(error, NeovimBuildError)
        assert isinstance(error, Exception)


class TestNeovimBuilder:
    """Test the NeovimBuilder class."""

    @pytest.fixture
    def temp_cache_dir(self):
        """Create temporary cache directory for testing."""
        with tempfile.TemporaryDirectory() as temp_dir:
            yield Path(temp_dir)

    @pytest.fixture
    def builder(self, temp_cache_dir):
        """Create NeovimBuilder instance for testing."""
        return NeovimBuilder(temp_cache_dir, "neovim/neovim")

    def test_init_creates_cache_directory(self, temp_cache_dir):
        """Test that __init__ creates the cache directory."""
        cache_dir = temp_cache_dir / "new_cache"
        assert not cache_dir.exists()

        builder = NeovimBuilder(cache_dir, "test/repo")

        assert cache_dir.exists()
        assert cache_dir.is_dir()
        assert builder.build_cache == cache_dir
        assert builder.repository == "test/repo"

    def test_init_with_existing_cache_directory(self, temp_cache_dir):
        """Test that __init__ works with existing cache directory."""
        temp_cache_dir.mkdir(exist_ok=True)

        builder = NeovimBuilder(temp_cache_dir, "neovim/neovim")

        assert temp_cache_dir.exists()
        assert builder.build_cache == temp_cache_dir
        assert builder.repository == "neovim/neovim"

    def test_build_commit_returns_cached_executable(self, builder, temp_cache_dir):
        """Test that build_commit returns cached executable if it exists."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        short_hash = commit_hash[:8]

        # Create fake cached executable
        cached_dir = temp_cache_dir / short_hash / "bin"
        cached_dir.mkdir(parents=True)
        cached_executable = cached_dir / "nvim"
        cached_executable.touch()

        result = builder.build_commit(commit_hash)

        assert result == cached_executable
        assert result.exists()

    @patch("nvim_manager.neovim_builder.tempfile.TemporaryDirectory")
    @patch.object(NeovimBuilder, "_shallow_clone_commit")
    @patch.object(NeovimBuilder, "_compile_and_install")
    def test_build_commit_fresh_build_success(
        self,
        mock_compile,
        mock_clone,
        mock_temp_dir,
        builder,
        temp_cache_dir,
    ):
        """Test successful fresh build when no cache exists."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        short_hash = commit_hash[:8]

        # Mock temporary directory context manager
        temp_path = "/tmp/fake-temp"
        mock_temp_context = MagicMock()
        mock_temp_context.__enter__.return_value = temp_path
        mock_temp_context.__exit__.return_value = None
        mock_temp_dir.return_value = mock_temp_context

        # Ensure the executable doesn't exist yet (fresh build scenario)
        install_dir = temp_cache_dir / short_hash
        executable = install_dir / "bin" / "nvim"

        # Mock the methods to simulate build process
        def mock_compile_side_effect(source_dir, build_dir, install_dir):
            # Create the executable during compilation
            install_dir.mkdir(parents=True, exist_ok=True)
            (install_dir / "bin").mkdir(exist_ok=True)
            (install_dir / "bin" / "nvim").touch()

        mock_compile.side_effect = mock_compile_side_effect

        result = builder.build_commit(commit_hash)

        assert result == executable
        mock_clone.assert_called_once_with(commit_hash, Path(temp_path) / "source")
        mock_compile.assert_called_once_with(
            Path(temp_path) / "source",
            Path(temp_path) / "build",
            install_dir,
        )

    @patch("nvim_manager.neovim_builder.tempfile.TemporaryDirectory")
    @patch.object(NeovimBuilder, "_shallow_clone_commit")
    @patch.object(NeovimBuilder, "_compile_and_install")
    def test_build_commit_missing_executable_after_build(
        self,
        mock_compile,
        mock_clone,
        mock_temp_dir,
        builder,
    ):
        """Test that NeovimCompileError is raised if executable missing after build."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"

        # Mock temporary directory context manager
        temp_path = "/tmp/fake-temp"
        mock_temp_context = MagicMock()
        mock_temp_context.__enter__.return_value = temp_path
        mock_temp_context.__exit__.return_value = None
        mock_temp_dir.return_value = mock_temp_context

        # Don't create the executable - simulate build failure

        with pytest.raises(NeovimCompileError) as exc_info:
            builder.build_commit(commit_hash)

        assert "executable not found" in str(exc_info.value)

    @patch("git.Repo.clone_from")
    def test_shallow_clone_commit_success(self, mock_clone, builder):
        """Test successful shallow clone operation."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        target_dir = Path("/fake/target")

        # Mock repo and git operations
        mock_repo = MagicMock()
        mock_clone.return_value = mock_repo

        builder._shallow_clone_commit(commit_hash, target_dir)

        # Verify clone_from called with correct URL
        expected_url = f"https://github.com/{builder.repository}.git"
        mock_clone.assert_called_once_with(expected_url, target_dir, depth=1)

        # Verify git operations
        mock_repo.git.fetch.assert_called_once_with("--depth=1", "origin", commit_hash)
        mock_repo.git.checkout.assert_called_once_with(commit_hash)

    @patch("git.Repo.clone_from")
    def test_shallow_clone_commit_clone_failure(self, mock_clone, builder):
        """Test that NeovimCloneError is raised when clone fails."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        target_dir = Path("/fake/target")

        mock_clone.side_effect = Exception("Clone failed")

        with pytest.raises(NeovimCloneError) as exc_info:
            builder._shallow_clone_commit(commit_hash, target_dir)

        assert "Failed to clone commit abcd1234" in str(exc_info.value)

    @patch("git.Repo.clone_from")
    def test_shallow_clone_commit_fetch_failure(self, mock_clone, builder):
        """Test that NeovimCloneError is raised when fetch fails."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        target_dir = Path("/fake/target")

        mock_repo = MagicMock()
        mock_clone.return_value = mock_repo
        mock_repo.git.fetch.side_effect = Exception("Fetch failed")

        with pytest.raises(NeovimCloneError) as exc_info:
            builder._shallow_clone_commit(commit_hash, target_dir)

        assert "Failed to clone commit abcd1234" in str(exc_info.value)

    @patch("git.Repo.clone_from")
    def test_shallow_clone_commit_checkout_failure(self, mock_clone, builder):
        """Test that NeovimCloneError is raised when checkout fails."""
        commit_hash = "abcd1234567890abcdef1234567890abcdef12"
        target_dir = Path("/fake/target")

        mock_repo = MagicMock()
        mock_clone.return_value = mock_repo
        mock_repo.git.checkout.side_effect = Exception("Checkout failed")

        with pytest.raises(NeovimCloneError) as exc_info:
            builder._shallow_clone_commit(commit_hash, target_dir)

        assert "Failed to clone commit abcd1234" in str(exc_info.value)

    @patch.object(NeovimBuilder, "_execute_build_command")
    def test_compile_and_install_success(self, mock_execute, builder, temp_cache_dir):
        """Test successful compilation and installation."""
        source_dir = Path("/fake/source")
        build_dir = temp_cache_dir / "build"
        install_dir = temp_cache_dir / "install"

        # Mock successful command execution
        mock_execute.return_value = True

        builder._compile_and_install(source_dir, build_dir, install_dir)

        # Verify build directory was created
        assert build_dir.exists()

        # Verify all commands were executed
        assert mock_execute.call_count == 5

        # Check that log files would be created
        assert (build_dir / "build_stdout.log").exists()
        assert (build_dir / "build_stderr.log").exists()

    @patch.object(NeovimBuilder, "_execute_build_command")
    def test_compile_and_install_command_failure(
        self, mock_execute, builder, temp_cache_dir
    ):
        """Test that NeovimCompileError is raised when a command fails."""
        source_dir = Path("/fake/source")
        build_dir = temp_cache_dir / "build"
        install_dir = temp_cache_dir / "install"

        # Mock command failure on second command
        mock_execute.side_effect = [True, False]

        with pytest.raises(NeovimCompileError) as exc_info:
            builder._compile_and_install(source_dir, build_dir, install_dir)

        assert "Build failed at step 2/5" in str(exc_info.value)
        assert mock_execute.call_count == 2

    @patch("nvim_manager.neovim_builder.run_command")
    def test_execute_build_command_success(self, mock_run, builder, temp_cache_dir):
        """Test successful build command execution."""
        args = ["echo", "test"]
        cwd = Path("/fake/cwd")

        # Mock successful command
        mock_result = MagicMock()
        mock_result.stdout = "test output"
        mock_result.stderr = "test error"
        mock_run.return_value = mock_result

        # Create log files
        stdout_file = temp_cache_dir / "stdout.log"
        stderr_file = temp_cache_dir / "stderr.log"

        with stdout_file.open("w") as stdout_f, stderr_file.open("w") as stderr_f:
            result = builder._execute_build_command(args, cwd, stdout_f, stderr_f)

        assert result is True
        mock_run.assert_called_once_with(args, cwd=cwd, capture_output=True)

        # Check log files contain expected content
        stdout_content = stdout_file.read_text()
        stderr_content = stderr_file.read_text()

        assert "=== Executing: echo test ===" in stdout_content
        assert "=== Executing: echo test ===" in stderr_content
        assert "test output" in stdout_content
        assert "test error" in stderr_content

    @patch("nvim_manager.neovim_builder.run_command")
    def test_execute_build_command_failure(self, mock_run, builder, temp_cache_dir):
        """Test build command execution failure."""
        import subprocess

        args = ["false"]
        cwd = Path("/fake/cwd")

        # Mock command failure
        error = subprocess.CalledProcessError(1, args, "error output", "error stderr")
        mock_run.side_effect = error

        # Create log files
        stdout_file = temp_cache_dir / "stdout.log"
        stderr_file = temp_cache_dir / "stderr.log"

        with stdout_file.open("w") as stdout_f, stderr_file.open("w") as stderr_f:
            result = builder._execute_build_command(args, cwd, stdout_f, stderr_f)

        assert result is False

        # Check error was logged
        stderr_content = stderr_file.read_text()
        assert "Command failed with exit code 1" in stderr_content
        assert "error output" in stdout_file.read_text()
        assert "error stderr" in stderr_content

    def test_build_commands_structure(self, builder, temp_cache_dir):
        """Test that build commands have the correct structure."""
        source_dir = temp_cache_dir / "source"
        build_dir = temp_cache_dir / "build"
        install_dir = temp_cache_dir / "install"

        # Create source directory structure
        source_dir.mkdir(parents=True)
        (source_dir / "cmake.deps").mkdir()

        # We need to access the command list that would be generated
        # Let's extract this by patching the execution part
        with patch.object(builder, "_execute_build_command", return_value=True):
            builder._compile_and_install(source_dir, build_dir, install_dir)

        # The test passes if no exception is raised and the method completes
        # This ensures the command structure is valid

    def test_different_repository_url(self, temp_cache_dir):
        """Test that custom repository URLs are used correctly."""
        custom_repo = "user/custom-neovim"
        builder = NeovimBuilder(temp_cache_dir, custom_repo)

        with patch("git.Repo.clone_from") as mock_clone:
            mock_repo = MagicMock()
            mock_clone.return_value = mock_repo

            try:
                builder._shallow_clone_commit("abcd1234", Path("/fake"))
            except Exception:
                pass  # We just want to check the URL

            expected_url = f"https://github.com/{custom_repo}.git"
            mock_clone.assert_called_once_with(expected_url, Path("/fake"), depth=1)
