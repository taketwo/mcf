# https://github.com/arximboldi/dotfiles/blob/master/emacs/.ycm_extra_conf.py
# https://github.com/Valloric/ycmd/blob/master/cpp/ycm/.ycm_extra_conf.py
# https://jonasdevlieghere.com/a-better-youcompleteme-config/

# Debugging tips:
#
# 1. Logs go to server stderr. Toggle logs window in a running Vim instance using the
#    :YcmToggleLogs command, then open a C++ file.

import re
import os
import os.path
import logging


SOURCES = ['.cpp', '.cxx', '.c', '.cc']
HEADERS = ['.h', '.hxx', '.hpp', '.hh']
BASE_FLAGS = ['-Wall', '-std=c++14', '-xc++', '-I/usr/lib/ -I/usr/include/']


def is_header_file(filename):
    extension = os.path.splitext(filename)[1]
    return extension in HEADERS


def make_relative_paths_in_flags_absolute(flags, working_directory):
    if not working_directory:
        return list(flags)
    new_flags = []
    make_next_absolute = False
    path_flags = [ '-isystem', '-I', '-iquote', '--sysroot=' ]
    for flag in flags.split(" "):
        new_flag = flag

        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith('/'):
                new_flag = os.path.join(working_directory, flag)

        for path_flag in path_flags:
            if flag == path_flag:
                make_next_absolute = True
                break

            if flag.startswith(path_flag):
                path = flag[ len(path_flag): ]
                new_flag = path_flag + os.path.join(working_directory, path)
                break

        if new_flag:
            new_flags.append(new_flag)
    return new_flags


def generate_corresponding_source_file_proposals(filename):
    # Generate paths for possibly correspondent source files for a given header.
    # First heuristic: same directory
    candidates = [filename]
    # Second heuristic: "src" subtree assuming flat library organization
    candidate, num = re.subn(r'include/[^/]*', 'src', filename)
    if num == 1:
        candidates.append(candidate)
    # Third heuristic: "src" subtree assuming modularized library organization
    candidate, num = re.subn(r'include/[^/]*/[^/]*', 'src', filename)
    if num == 1:
        candidates.append(candidate)
    for candidate in candidates:
        basename = os.path.splitext(candidate)[0]
        for extension in SOURCES:
            yield basename + extension


def try_find_ros_compilation_database(filename):
    build_path = find_catkin_tools_build(filename) or find_catkin_make_build(filename)
    if build_path is not None:
        candidate = os.path.join(build_path, 'compile_commands.json')
        if os.path.isfile(candidate):
            logging.info("Found ROS compilation database for " + filename + " at " + candidate)
            return candidate
    return None


def find_catkin_tools_build(filename):
    try:
        from catkin_tools.metadata import find_enclosing_workspace
        from catkin_tools.context import Context
        import rospkg
        workspace = find_enclosing_workspace(filename)
        ctx = Context.load(workspace, {}, {}, load_env=False)
        package = rospkg.get_package_name(filename)
        path = os.path.join(ctx.build_space_abs, package)
        if os.path.isdir(path):
            logging.info("Found catkin tools build directory at " + path)
            return path
    except ImportError as e:
        logging.warn("Failed to import modules to perform catkin tools build discovery: {}".format(e))
    except:
        pass
    return None


def find_catkin_make_build(filename):
    from pathlib import Path
    for p in Path(filename).parents:
        tag = p / ".catkin_workspace"
        if tag.is_file():
            for b in ("build", "build_reldebug", "build_debug", "build_release"):
                build_path = p / b
                if build_path.is_dir():
                    return build_path
    return None


def find_nearest(path, target):
    candidate = os.path.join(path, target)
    build_candidate = os.path.join(path, 'build', target)
    if os.path.isfile(candidate) or os.path.isdir(candidate):
        logging.info("Found nearest " + target + " at " + candidate)
        return candidate
    elif os.path.isfile(build_candidate) or os.path.isdir(build_candidate):
        logging.info("Found nearest " + target + " at " + build_candidate)
        return build_candidate
    else:
        parent = os.path.dirname(os.path.abspath(path))
        if(parent == path):
            raise RuntimeError("Could not find " + target)
        return find_nearest(parent, target)


def flags_from_include_directory(root):
    try:
        include_path = find_nearest(root, 'include')
        flags = []
        for dirroot, dirnames, filenames in os.walk(include_path):
            for dir_path in dirnames:
                real_path = os.path.join(dirroot, dir_path)
                flags = flags + ["-I" + real_path]
        return flags
    except:
        return None


class CompilationDatabase:
    def __init__(self, compilation_db_path):
        import json
        data = json.load(open(compilation_db_path, "r"))
        self.db = {k["file"]: self.extract_flags(k["command"]) for k in data}

    def get_flags_for_file(self, filename):
        logging.info("Extracting flags from compilation database for " + filename)
        # The compilation_commands.json file generated by CMake does not have
        # entries for header files. We try to find corresponding source file by
        # grepping for the base name. If one exists, the flags for that file
        # should be good enough.
        if is_header_file(filename):
            logging.info(filename + " is a header file, using heuristics to find flags")
            for replacement_file in generate_corresponding_source_file_proposals(filename):
                if replacement_file in self.db.keys():
                    logging.info('Using flags from a corresponding source file')
                    return self.db[replacement_file]
            # No source file with corresponding name found. Now we check for any source
            # file in the same (or sibling "src") directory:
            directory = re.sub(r'include/[^/]*', 'src', os.path.dirname(filename))
            some_source_file = self.get_some_file_from_database(directory)
            if some_source_file:
                logging.info('Using flags from a file in the same directory')
                return self.db[some_source_file]
            logging.info('Using flags from arbitrary file in compilation DB')
            return self.db.items()[0]
        return self.db[filename]

    def get_some_file_from_database(self, directory):
        for key in self.db.keys():
            if key.startswith(directory):
                return key
        return None

    def extract_flags(self, command):
        # TODO may need to make paths in include flags absolute
        flags = list()
        tokens = command.split(" ")
        skip_next = True  # because the first item is compiler binary
        for t in tokens:
            if t in ("-o", "-c"):
                skip_next = True
                continue
            if not skip_next and t:
                flags.append(t)
            skip_next = False
        return flags

    def __bool__(self):
        return bool(self.db)


def flags_from_compilation_database(root, filename):
    try:
        compilation_db_path = try_find_ros_compilation_database(filename)
        if compilation_db_path is None:
            compilation_db_path = find_nearest(root, 'compile_commands.json')
        compilation_db = CompilationDatabase(compilation_db_path)
        if not compilation_db:
            logging.warn("Compilation database file found but unable to load")
            return None
        flags = compilation_db.get_flags_for_file(filename)
        if not flags:
            logging.info("No flags for " + filename + " in compilation database")
            return None
        return flags
    except Exception as e:
        logging.exception(e)
        return None


def flags_for_file(filename):
    logging.info("Flags for " + filename + " requested")
    root = os.path.realpath(filename)
    compilation_db_flags = flags_from_compilation_database(root, filename)
    if compilation_db_flags:
        logging.info("Collected flags from compilation DB: " + str(compilation_db_flags))
        final_flags = compilation_db_flags + ['-xc++']
    else:
        final_flags = BASE_FLAGS
        include_flags = flags_from_include_directory(root)
        if include_flags:
            logging.info("Collected flags from include directory: " + str(include_flags))
            final_flags = final_flags + include_flags
    return { 'flags': final_flags, 'do_cache': True }


def Settings(**kwargs):
    if kwargs["language"] == 'cfamily':
        return flags_for_file(kwargs["filename"])
    return {}
