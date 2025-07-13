"""Tests for EditorManager class and related functions."""

from nvim_manager.editor_manager import parse_neovim_version_string


class TestParseNeovimVersionString:
    """Test the parse_neovim_version_string function."""

    def test_nightly_build_with_plus_g_prefix(self):
        """Test parsing nightly build with +g prefix."""
        version_output = "NVIM v0.10.0-dev-3145+g9ca81b025\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "9ca81b025"

    def test_local_build_with_dev_prefix(self):
        """Test parsing local build with -dev- prefix."""
        version_output = "NVIM v0.10.0-dev-a0a189a\nBuild type: Debug"
        result = parse_neovim_version_string(version_output)
        assert result == "a0a189a"

    def test_longer_commit_hash_nightly(self):
        """Test parsing with longer commit hash in nightly format."""
        version_output = "NVIM v0.11.0-dev-1234+g1a2b3c4d5e6f7890\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "1a2b3c4d5e6f7890"

    def test_longer_commit_hash_local(self):
        """Test parsing with longer commit hash in local format."""
        version_output = "NVIM v0.11.0-dev-1a2b3c4d5e6f7890\nBuild type: Debug"
        result = parse_neovim_version_string(version_output)
        assert result == "1a2b3c4d5e6f7890"

    def test_stable_release_no_commit(self):
        """Test parsing stable release without commit hash."""
        version_output = "NVIM v0.9.0\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result is None

    def test_prerelease_no_commit(self):
        """Test parsing prerelease without commit hash."""
        version_output = "NVIM v0.9.0-rc1\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result is None

    def test_malformed_version_string(self):
        """Test parsing malformed version string."""
        version_output = "Something went wrong"
        result = parse_neovim_version_string(version_output)
        assert result is None

    def test_empty_string(self):
        """Test parsing empty string."""
        version_output = ""
        result = parse_neovim_version_string(version_output)
        assert result is None

    def test_single_word_version(self):
        """Test parsing single word version."""
        version_output = "NVIM"
        result = parse_neovim_version_string(version_output)
        assert result is None

    def test_version_line_with_extra_spaces(self):
        """Test parsing version line with extra spaces."""
        # Extra spaces break the simple split logic, so this should fail
        version_output = "NVIM  v0.10.0-dev-3145+g9ca81b025  \nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result is None  # Multiple spaces break the parsing

    def test_multiline_output_uses_first_line(self):
        """Test that only the first line is parsed."""
        version_output = """NVIM v0.10.0-dev-3145+g9ca81b025
LuaJIT 2.1.0-beta3
Compilation: clang version 14.0.0
Features: +acl +iconv +tui
"""
        result = parse_neovim_version_string(version_output)
        assert result == "9ca81b025"

    def test_mixed_case_commit_hash(self):
        """Test parsing with mixed case commit hash."""
        # The regex only matches [0-9a-f], so uppercase letters are not matched
        version_output = "NVIM v0.10.0-dev-3145+gABCdef123\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "3145"  # Falls back to -dev- pattern, stops at uppercase

    def test_short_commit_hash_nightly(self):
        """Test parsing with short commit hash in nightly format."""
        version_output = "NVIM v0.10.0-dev-1+gabc123\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "abc123"

    def test_short_commit_hash_local(self):
        """Test parsing with short commit hash in local format."""
        version_output = "NVIM v0.10.0-dev-abc123\nBuild type: Debug"
        result = parse_neovim_version_string(version_output)
        assert result == "abc123"

    def test_nightly_takes_precedence_over_dev(self):
        """Test that +g pattern takes precedence over -dev- pattern."""
        # This is an edge case that shouldn't happen in practice, but tests priority
        version_output = "NVIM v0.10.0-dev-abc123+gdef456\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "def456"

    def test_numeric_only_commit_hash(self):
        """Test parsing with numeric-only commit hash."""
        version_output = "NVIM v0.10.0-dev-3145+g1234567890\nBuild type: Release"
        result = parse_neovim_version_string(version_output)
        assert result == "1234567890"

    def test_alpha_only_commit_hash(self):
        """Test parsing with alphabetic-only commit hash."""
        # The regex [0-9a-f]+ stops at 'g' since it's not in hex range
        version_output = "NVIM v0.10.0-dev-abcdefgh\nBuild type: Debug"
        result = parse_neovim_version_string(version_output)
        assert result == "abcdef"  # Stops at 'g' which is not a hex character
