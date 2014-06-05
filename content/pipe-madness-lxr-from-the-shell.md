Title: Pipe Madness: LXR from the Shell
Date: 2014-06-05 13:50
Author: sulami
Category: Linux
Tags: pipemadness, cli, kernel, lxr

Well, it is not really [LXR](http://lxr.free-electrons.com/ident), but it works
on Linux-Next without actually having to use LXR and does something similar.
The rather short pipe I use here is grepping a directory for a keyword and
using awk and finally sort to present me with an ordered list of files which
contain my keyword.

    grep "keyword" -r ./*.c -c | grep -v 'Binary' | awk -F: '{if ($2!="0") print $2" "$1}' | sort -nr

