Plugin for CudaText.
Allows to manage "projects" (files have .cuda-proj extension).
Plugin shows "Project" panel in the side panel, with context menu.

Context menu has items:
- Project file
   - New project
   - Open project...
   - Recent projects
   - Save project as...
- Root nodes
   - Add folder...
   - Add file...
   - Clear project
   - Remove node
- Refresh
- Go to file...
- Project properties
- Config...

Tech notes:

- You can add only root nodes to tree-view. Project files store only links for root-nodes (files and dirs). File-list of folders is not saved to project. So project files are small.
- When you call "Remove node", node on top project level is removed, and selection inside that node is ignored.
- You can open project files from Open dialog, or command line. Instead of opening raw JSON content, project will be loaded.

Authors:
- Nikita Melentev https://github.com/pohmelie
- Alexey Torgashin (CudaText)
License: MIT
