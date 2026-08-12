"""Tests for NeovimBuilder class and related functions."""

from nvim_manager.neovim_builder import _is_network_failure


class TestIsNetworkFailure:
    """Test the _is_network_failure function."""

    def test_cmake_external_project_download_failure(self):
        """Test detection of a CMake ExternalProject download failure."""
        output = """
CMake Error at treesitter_vim-stamp/download-treesitter_vim.cmake:170 (message):
  Each download failed!

    error: downloading 'https://github.com/tree-sitter-grammars/tree-sitter-vim/archive/v0.8.1.tar.gz' failed
        status_code: 56
        status_string: "Failure when receiving data from the peer"
"""
        assert _is_network_failure(output) is True

    def test_connection_died_retry(self):
        """Test detection of a dropped HTTP/2 connection."""
        output = "REFUSED_STREAM, retrying a fresh connect\n\nConnection died, retrying a fresh connect (retry count: 1)\n"
        assert _is_network_failure(output) is True

    def test_dns_resolution_failure(self):
        """Test detection of a DNS resolution failure."""
        output = "curl: (6) Could not resolve host: github.com"
        assert _is_network_failure(output) is True

    def test_genuine_compile_error_is_not_network_failure(self):
        """Test that a real compilation error is not misclassified as network."""
        output = """
[42/120] Building C object src/nvim/CMakeFiles/nvim.dir/eval.c.o
FAILED: src/nvim/CMakeFiles/nvim.dir/eval.c.o
/usr/bin/cc ... -c ../src/nvim/eval.c
../src/nvim/eval.c:123:5: error: use of undeclared identifier 'foo'
"""
        assert _is_network_failure(output) is False

    def test_empty_output_is_not_network_failure(self):
        """Test that empty output is not classified as a network failure."""
        assert _is_network_failure("") is False
