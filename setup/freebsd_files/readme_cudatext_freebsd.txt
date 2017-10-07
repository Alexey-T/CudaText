How to use CudaText on FreeBSD 11.1 64-bit

1) If you run "cudatext" file, it will show msg about missing "libiconv.so.3", 
so you need to create a symlink to "libiconv.so.2", which fixes this:

# ln -s /usr/local/lib/libiconv.so.2 /usr/local/lib/libiconv.so.3


2) How to use Python3 engine
I found libpython3* in /usr/local/lib, it is folder in PATH,
so this line in "user.json" uses it:

  "pylib__freebsd": "libpython3.6m.so",

Restart CudaText, Addon Manager works then, and all ok.
