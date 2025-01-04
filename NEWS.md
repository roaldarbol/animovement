# animovement 0.6.0

# animovement 0.5.1
Previously, `smooth_track` was only built to smoothen trackball data. In this fix, we fixed it by doing the following:
- Changed the name to `smooth_movement`
- Added parameter `use_derivatives` (default: `FALSE`) which is how trackball data should be smoothed
- Written the non-derivative code

So now `smooth_movement` should be able to smooth all your movement data!

# animovement 0.5.0
A big update! There are three major updates:
- We finally stabilised on a **data format** and **implemented it for all readers**! 
- We've introduced **metadata**. It's still experimental, but an important step in the right direction.
- We added tests to ensure the output data frames from readers always contain the expected columns and column classes.
- We also added tests to ensure metadata is present after reading a data frame.

We are not nearly done with the work on metadata, but are exited to finally get it started.

# animovement 0.4.1

Changed parameter in `read_animalta()`.

# animovement 0.4.0

Added readers for AnimalTA (`read_animalta`) and idtracker.ai (`read_idtracker`).

# animovement 0.3.0

Has added the ability to read centroid tracking from Bonsai files through `read_bonsai()`.

# animovement 0.2.0

**Package name changed to `animovement`!** To reflect the change in scope of the package to now cover a wide variety of movement data, the package has changed its name. This was a conscious decision as maintaining two packages with similar functionality wouldn't be feasible long-term. The new update brings with it a wealth of new functionality, but also a lot of breaking changes. With this update, the package aligns much closer to my vision for it, and I expect no more major breaking changes in the near future. So let's delve in to all the new stuff!

* Much improved documentation
* Added new readers `read_deeplabcut()`, `read_sleap()`, `read_trex()`...
* Standardised data cleaning with `smooth_tracks()`
* 
* More tests

Although this is annoying to current users, it was a necessity to ensure proper testing of all the various functions which have now been broken into smaller subsets. If you still need the former `trackballr` package functionality, you can download the source files in Github and [install from source](https://stackoverflow.com/a/1474125).

If you encounter bugs, please report them in the [Github issues](https://github.com/roaldarbol/animovement/issues).

# trackballr 0.1.2

* Added tests for `read_trackball_data`
* `format` was deprecated as a parameter in `read_trackball_data`

# trackballr 0.1.1

* Added sample data
* Using sample data in the `README` EXAMPLE.
* Changed `configuration` in `read_trackball_data` to `fixed`/`free`.

# trackballr 0.1.0

* Initial upload to Github.
