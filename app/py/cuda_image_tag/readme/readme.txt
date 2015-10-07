Plugin "Image Tag" for CudaText.
It gives command to choose image file (jpeg, png, gif), and inserts image info into code. 

For CSS lexer (and several CSS based lexers: scss, sass, stylus) it inserts such text:

    background: url("path/file.png");
    width: 70px;
    height: 70px;

For other lexers (assumed it's HTML) it inserts HTML <img> tag:

    <img src="path/file.png" width="70" height="70" alt="untitled" />

It replaces full path of image to short path "fn.png" or "subfolder/fn.png"
if image file is in editor file's subfolder.

Author: Alexey T.
