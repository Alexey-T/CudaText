# The 'libs' subpackage of the Project Manager plugin: the home of
# the vendored third-party libraries. Its packages are loaded as
# PRIVATE subpackages of the plugin (cuda_project_man.libs.xxx)
# through the plugin's own package path - nothing is ever added to
# sys.path, and no top-level module is registered (see
# projman_trash.py / projman_os_sort.py for the loaders).
# 'libs/natsort' - the natsort library (MIT license), the engine of
# the tree's OS file-browser ordering of "sort by name" (its
# internal imports are relative, like send2trash's).
# 'libs/send2trash' - the Send2Trash library (BSD license), the
# engine of the trash (Recycle Bin / Trash) support of the delete
# commands.
