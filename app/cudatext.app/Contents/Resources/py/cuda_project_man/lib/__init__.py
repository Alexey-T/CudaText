# The 'lib' subpackage of the Project Manager plugin: the home of the
# vendored third-party libraries. Its packages are loaded as PRIVATE
# subpackages of the plugin (cuda_project_man.lib.xxx) through the
# plugin's own package path - nothing is ever added to sys.path, and no
# top-level module is registered (see projman_trash.py for the loader).
# 'lib/send2trash' - the Send2Trash library (BSD license), the engine
# of the trash (Recycle Bin / Trash) support of the delete commands.
