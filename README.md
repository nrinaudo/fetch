fetch
=====

Simple Scala wrapper for `java.net.URLConnection`.

This is still in the very early stages and should not be used in a production environment.

I created this because while I rather like [Dispatch](https://github.com/dispatch/reboot), the various services I
write and maintain have and expect proper keep-alive etiquette, and enforce it by dropping connections when too many
are opened too fast - something that Dispatch does rather a lot.
