"""Tests for NeovimBuilder class."""

from unittest.mock import MagicMock, patch

import pytest

from nvim_manager.neovim_builder import NeovimBuilder


@pytest.fixture
def build_cache(tmp_path):
    """Create a temporary build cache directory."""
    return tmp_path / "build_cache"


class TestNeovimBuilder:
    """Test NeovimBuilder public interface."""

    def test_constructor(self, build_cache):
        """Test NeovimBuilder initialization."""
        builder = NeovimBuilder(build_cache)

        assert builder.build_cache == build_cache

    def test_build_commit_cache_hit(self, build_cache):
        """Test build_commit returns cached executable when it exists."""
        # Setup: create actual directories and executable
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        expected_path = build_cache / "abc123de" / "bin" / "nvim"

        # Create the cached executable
        expected_path.parent.mkdir(parents=True, exist_ok=True)
        expected_path.touch()

        # Test: should return cached path without building
        result = builder.build_commit(commit_hash)

        # Verify: returns expected path
        assert result == expected_path

    @patch("nvim_manager.neovim_builder.NeovimBuilder._download_and_build")
    def test_build_commit_full_build(self, mock_download_build, build_cache):
        """Test build_commit performs full build when executable doesn't exist."""
        # Setup: mock build process
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        expected_path = build_cache / "abc123de" / "bin" / "nvim"

        # Mock _download_and_build to create the executable
        def mock_build_side_effect(commit_hash, install_dir):
            expected_path.parent.mkdir(parents=True, exist_ok=True)
            expected_path.touch()

        mock_download_build.side_effect = mock_build_side_effect

        # Test: should perform full build
        result = builder.build_commit(commit_hash)

        # Verify: build was called and correct path returned
        mock_download_build.assert_called_once_with(
            commit_hash, build_cache / "abc123de"
        )
        assert result == expected_path

    @patch("nvim_manager.neovim_builder.NeovimBuilder._download_and_build")
    def test_build_commit_build_failure(self, mock_download_build, build_cache):
        """Test build_commit raises RuntimeError when build fails."""
        # Setup: mock build failure (don't create executable)
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"

        # Mock _download_and_build does NOT create the executable (simulating failure)
        mock_download_build.return_value = None

        # Test: should raise RuntimeError when executable not found after build
        with pytest.raises(
            RuntimeError,
            match="Build completed but executable not found",
        ):
            builder.build_commit(commit_hash)

        # Verify: build was attempted
        mock_download_build.assert_called_once_with(
            commit_hash, build_cache / "abc123de"
        )

    @patch("requests.get")
    @patch("zipfile.ZipFile")
    @patch("tempfile.NamedTemporaryFile")
    def test_download_source_zip_success(
        self, mock_temp_file, mock_zipfile, mock_get, build_cache, tmp_path
    ):
        """Test successful source ZIP download and extraction."""
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        source_dir = tmp_path / "source"

        # Mock HTTP response
        mock_response = MagicMock()
        mock_response.content = b"fake zip content"
        mock_response.raise_for_status.return_value = None
        mock_get.return_value = mock_response

        # Mock temporary file
        mock_temp_file_instance = MagicMock()
        mock_temp_file_instance.name = "/tmp/fake.zip"
        mock_temp_file.return_value.__enter__.return_value = mock_temp_file_instance

        # Mock ZIP extraction
        mock_zip = MagicMock()
        mock_zipfile.return_value.__enter__.return_value = mock_zip

        # Create fake extracted directory
        extracted_dir = source_dir.parent / "neovim-abc123de"
        extracted_dir.mkdir(parents=True, exist_ok=True)

        # Test
        builder._download_source_zip(commit_hash, source_dir)

        # Verify: HTTP request made correctly
        mock_get.assert_called_once_with(
            f"https://api.github.com/repos/neovim/neovim/zipball/{commit_hash}",
            timeout=30,
        )

        # Verify: ZIP extraction attempted
        mock_zip.extractall.assert_called_once()

    @patch("requests.get")
    def test_download_source_zip_http_error(self, mock_get, build_cache, tmp_path):
        """Test download failure when HTTP request fails."""
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        source_dir = tmp_path / "source"

        # Mock HTTP error
        mock_response = MagicMock()
        mock_response.raise_for_status.side_effect = Exception("HTTP Error")
        mock_get.return_value = mock_response

        # Test: should raise exception
        with pytest.raises(Exception, match="HTTP Error"):
            builder._download_source_zip(commit_hash, source_dir)

    @patch("nvim_manager.neovim_builder.run_command")
    def test_build_and_install_success(self, mock_run_command, build_cache, tmp_path):
        """Test successful build and install process."""
        builder = NeovimBuilder(build_cache)
        source_dir = tmp_path / "source"
        build_dir = tmp_path / "build"
        install_dir = tmp_path / "install"
        short_hash = "abc123de"

        # Create source and build directory structure
        source_dir.mkdir(parents=True)
        build_dir.mkdir(parents=True)
        (source_dir / "cmake.deps").mkdir()

        # Mock successful command execution
        mock_run_command.return_value = MagicMock(stdout="", stderr="")

        # Test
        result = builder._build_and_install(
            source_dir, build_dir, install_dir, short_hash
        )

        # Verify: all cmake commands were called
        assert result is True
        assert mock_run_command.call_count == 5  # 5 cmake commands

    @patch("nvim_manager.neovim_builder.run_command")
    def test_build_and_install_failure(self, mock_run_command, build_cache, tmp_path):
        """Test build failure when cmake command fails."""
        builder = NeovimBuilder(build_cache)
        source_dir = tmp_path / "source"
        build_dir = tmp_path / "build"
        install_dir = tmp_path / "install"
        short_hash = "abc123de"

        # Create source and build directory structure
        source_dir.mkdir(parents=True)
        build_dir.mkdir(parents=True)
        (source_dir / "cmake.deps").mkdir()

        # Mock command failure
        from subprocess import CalledProcessError

        mock_run_command.side_effect = CalledProcessError(1, "cmake", "", "")

        # Test
        result = builder._build_and_install(
            source_dir, build_dir, install_dir, short_hash
        )

        # Verify: build failed
        assert result is False
