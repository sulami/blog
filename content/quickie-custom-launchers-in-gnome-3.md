Title: Quickie: custom launchers in Gnome 3
Date: 2014-03-17 22:26
Author: sulami
Category: Linux
Tags: quickie, gnome
Slug: quickie-custom-launchers-in-gnome-3

Let's say you are using Gnome 3, because it is actually sort of usable
with some extensions and the alternatives are not to shiny. Let's say
you want to add some obscure program to the dash or change the icon or
the startup parameters of an existing one. Let's say, I know how:

Navigate to *\~/.local/share/applications*, and write a new file named
*yourprogram.desktop*. Fill it with conent like the following:

    {.lang:default .decode:true}
    [Desktop Entry]
    Name=My Program
    GenericName=Some Program
    Exec=program --foo
    Icon=program
    Terminal=false
    Type=Application

If you want some sort of custom icon, like a Faenza icon for
virt-manager (which for some reason does not exist), you can check at
*\~/.icons* for icons. Just name the filename without extension in your
shortcut.
