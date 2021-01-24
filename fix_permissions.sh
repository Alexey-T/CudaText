#!/bin/bash
find . -type f -exec chmod 644 {} +
find . -type d -exec chmod 755 {} +
find . -name "*.sh"      -type f -exec chmod 755 {} +
find . -name "*.command" -type f -exec chmod 755 {} +
find . -name "cudatext"  -type f -exec chmod 755 {} +
