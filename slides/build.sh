#!/bin/sh

pandoc -t beamer quickcheck.md -V theme:Singapore -V colortheme:whale -o quickcheck.pdf

