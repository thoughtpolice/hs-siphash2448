# Minimal package for siphash message-authentication codes

[![Build Status](https://travis-ci.org/thoughtpolice/hs-siphash2448.png?branch=master)](https://travis-ci.org/thoughtpolice/hs-siphash2448)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package implements minimal bindings to
[siphash](https://131002.net/siphash/) for use as a
message-authentication code (MAC.) It should be relatively easy to
both depend on, or include outright in your executable/package itself.

The underlying implementation is the `little` code of `siphash24` and
`siphash48` from [SUPERCOP][], which was originally implemented by Dan
J. Bernstein.

[siphash]: https://131002.net/siphash/
[SUPERCOP]: http://bench.cr.yp.to/supercop.html

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install siphash2448
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-siphash2448.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-siphash2448.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-siphash2448/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-siphash2448/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/hs-siphash2448/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-siphash2448/issues
[gh]: http://github.com/thoughtpolice/hs-siphash2448
[bb]: http://bitbucket.org/thoughtpolice/hs-siphash2448
[Hackage]: http://hackage.haskell.org/package/siphash2448
