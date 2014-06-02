Title: Pipe Madness: Finding Kernel Slackers
Date: 2014-06-02 12:55
Author: sulami
Category: Linux
Tags: pipemadness, cli, kernel

This is going to be a new series in which I showcase to horrible abominations
of shell pipelines I make up on the fly while working. This one is for finding
files in the Linux Kernel that produce extraordinary many warnings and errors
when pushed into checkpatch.pl, the kernel stylecheck script. Here we go:

    find drivers/staging/ -iname "\*.c" | xargs scripts/checkpatch.pl --terse -f | cut -d ":" -f 1 | uniq -c | grep -v 'total' | sort -n

This one runs on drivers/staging and sorts the worst files last. Also, it only
checks .c files. Takes some minutes to run on my Haswell Xeon with a SSD and
plenty of RAM.

