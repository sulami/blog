Title: Quickie: force-test virtualhosts
Date: 2014-03-17 01:22
Author: sulami
Category: Web
Tags: quickie, apache, nginx, wget
Slug: quickie-force-test-virtualhosts

I know, nginx does not have virtualhosts, it has got server blocks, but
most people will know what I mean. You define a new virtualhost/server
block and point it to your page. But to test it, you usually need to
update the DNS-records, and this may take hours, depending on your
situation. But you can force a specific host using wget (or curl, but
curl is not able to recursively download whole pages).

This is quite easy and can be accomplished by manually setting the
Host-variable in the HTTP-header, like this:

    {.lang:default .decode:true}
    wget -R --header="Host: www.google.com" 127.0.0.1

This would trick your local webserver into thinking you resolved
google.com to reach it, and therefore returns the appropriate page.
