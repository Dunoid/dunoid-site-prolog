#Dunoid's Prolog Site

This is a simple site for personal use, but it's quickly becoming a simple blogging toolkit.

This site is currently up (in a very limited state) at [Dunoid.org](http://dunoid.org).

## Reading the source code
`server.pl` is the root of the site, and it uses `templates.pl` for HTML templates and
`server_io.pl` for communicating to the database (which is just a persistent Prolog 
dataset, since it's such a small dataset).  `pages.pl` is used by `templates.pl` to
get the entries for paginated content.
