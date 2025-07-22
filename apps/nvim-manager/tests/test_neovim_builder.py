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
        assert builder.source_dir == build_cache / "neovim"

    @patch("nvim_manager.neovim_builder.NeovimBuilder._ensure_source_at_commit")
    def test_build_commit_cache_hit(self, mock_ensure_source, build_cache):
        """Test build_commit returns cached executable when it exists."""
        # Setup: create actual directories and executable
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        expected_path = build_cache / "installs" / "abc123de" / "bin" / "nvim"

        # Create the cached executable
        expected_path.parent.mkdir(parents=True, exist_ok=True)
        expected_path.touch()

        # Test: should return cached path without building
        result = builder.build_commit(commit_hash)

        # Verify: returns expected path and source was ensured
        assert result == expected_path
        mock_ensure_source.assert_called_once_with(commit_hash)

    @patch("tempfile.TemporaryDirectory")
    @patch("nvim_manager.neovim_builder.NeovimBuilder._build_and_install")
    @patch("nvim_manager.neovim_builder.NeovimBuilder._ensure_source_at_commit")
    def test_build_commit_full_build(
        self,
        mock_ensure_source,
        mock_build_install,
        mock_temp_dir,
        build_cache,
    ):
        """Test build_commit performs full build when executable doesn't exist."""
        # Setup: mock build process
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"
        expected_path = build_cache / "installs" / "abc123de" / "bin" / "nvim"

        # Mock temporary directory
        mock_temp_dir_instance = MagicMock()
        mock_temp_dir_instance.__enter__.return_value = "/tmp/test-build-dir"
        mock_temp_dir.return_value = mock_temp_dir_instance

        # Mock build_and_install to create the executable
        def mock_build_side_effect(*args):
            expected_path.parent.mkdir(parents=True, exist_ok=True)
            expected_path.touch()

        mock_build_install.side_effect = mock_build_side_effect

        # Test: should perform full build
        result = builder.build_commit(commit_hash)

        # Verify: all build steps called and correct path returned
        mock_ensure_source.assert_called_once_with(commit_hash)
        mock_temp_dir.assert_called_once()
        mock_build_install.assert_called_once()
        assert result == expected_path

    @patch("tempfile.TemporaryDirectory")
    @patch("nvim_manager.neovim_builder.NeovimBuilder._build_and_install")
    @patch("nvim_manager.neovim_builder.NeovimBuilder._ensure_source_at_commit")
    def test_build_commit_build_failure(
        self,
        mock_ensure_source,
        mock_build_install,
        mock_temp_dir,
        build_cache,
    ):
        """Test build_commit raises RuntimeError when build fails."""
        # Setup: mock build failure
        builder = NeovimBuilder(build_cache)
        commit_hash = "abc123def456"

        # Mock temporary directory
        mock_temp_dir_instance = MagicMock()
        mock_temp_dir_instance.__enter__.return_value = "/tmp/test-build-dir"
        mock_temp_dir.return_value = mock_temp_dir_instance

        # Mock build_and_install does NOT create the executable (simulating failure)

        # Test: should raise RuntimeError when executable not found after build
        with pytest.raises(
            RuntimeError,
            match="Build completed but executable not found",
        ):
            builder.build_commit(commit_hash)

        # Verify: build was attempted
        mock_ensure_source.assert_called_once_with(commit_hash)
        mock_build_install.assert_called_once()
