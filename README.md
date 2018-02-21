iexportplaylists
===

If you live and die by your iTunes playlists
like I do, fresh installing OS X can be a
herculean effort because it, heretofore, required
manual export and re-import of all playlists. This
program provides the ability to programmatically
back up all playlists (in re-importable XML) and
export an informal human-readable representation.

### Prerequisite
Newer versions of iTunes do not, by default, export
iTunes Library.xml, which this application depends on.
Getting iTunes to export this is very easy, just
[follow these instructions](https://support.apple.com/en-ie/HT201610)

### Installation
This was somewhat of a personal project so I can learn
to use Common Lisp but I'd love for you, dear reader,
to be able to use it if you're interested. There are
a couple of ways to install it...

First of all, you need a Common Lisp implementation.
I use [SBCL](http://www.sbcl.org) and
[Clozure Common Lisp](http://ccl.clozure.com) mainly
and this application was tested with both. These are both
high-performance, well-regarded implementations and they
both come packaged with [ASDF](https://common-lisp.net/project/asdf/)
which makes installation of this app easier.

#### the quicklisp way
First, install [quicklisp](https://www.quicklisp.org/beta/) following
the instructions on the website. Then,
`git clone https://github.com/tonyfischetti/iexportplaylists.git`
into the folder into your $HOME/quicklisp/local-projects directory
(where "$HOME" is your home directory.

Open up a lisp session, and execute:

```
(ql:quickload "iexportplaylists")
```

This should automagically download and install this apps dependencies:
- [cl-ppcre](http://weitz.de/cl-ppcre/)
- [cxml](https://common-lisp.net/project/cxml/)
- [xpath](https://common-lisp.net/project/plexippus-xpath/examples.html)
- [drakma](http://weitz.de/drakma/)


#### the ASDF way
Quicklisp uses ASDF internally but, for whatever reason, if you'd
like to not use quicklisp, you have to install the 4 dependencies listed
above--probably by downloading them, copying or sym-linking them to
the path that is given by running `asdf:*central-registry*` in your
lisp interpreter. Then, do the same for this package and run:

```
(asdf:operate 'asdf:load-op :iexportplaylists)
```

On SBCL and Clozure, the following incantation should work, too:

```
(require :iexportplaylists)
```


#### the easiest (but you're-gonna-have-to-trust-me) way
If anyone wants to use this application but can't follow my poor
instructions, you can contact me and I can try to make a
stand-alone executable for your system.


### Running it
After the `iexportplaylists` package is loaded, run

```
(iexportplaylists:main)
```

To exclude certain playlists from being exported, add them to
the variable `iexportplaylists:*ignored-playlists*`.
