## What is it?

This [Elm](http://elm-lang.org) package provides functions to create and
manipulate SVG elements using the [`opensolid/geometry`](http://package.elm-lang.org/packages/opensolid/geometry/latest)
data types. You can:

  - Draw 2D `opensolid/geometry` objects as SVG
  - Apply OpenSolid-based 2D transformations to arbitrary SVG elements
  - Convert SVG between different coordinate systems

## Drawing

The `lineSegment2d`, `triangle2d`, `polyline2d`, `polygon2d`, `circle2d`,
`ellipse2d`, `arc2d`, `ellipticalArc2d`, `quadraticSpline2d`, `cubicSpline2d`,
`point2d`, `vector2d`, `direction2d`, `boundingBox2d` and `text2d` functions all
produce standard [`Svg msg`](http://package.elm-lang.org/packages/elm-
lang/svg/latest/Svg#Svg) values that can be included in any SVG diagram:

![lineSegment2d](https://opensolid.github.io/images/svg/1.0/lineSegment2d.svg)
![triangle2d](https://opensolid.github.io/images/svg/1.0/triangle2d.svg)
![polyline2d](https://opensolid.github.io/images/svg/1.0/polyline2d.svg)
![polygon2d](https://opensolid.github.io/images/svg/1.0/polygon2d.svg)
![circle2d](https://opensolid.github.io/images/svg/1.0/circle2d.svg)

The appearance of the resulting elements can be customized by adding SVG
attributes such as `fill` and `stroke`.

## Transformation

The `scaleAbout`, `rotateAround`, `translateBy` and `mirrorAcross` functions
behave just like their standard OpenSolid counterparts. You can use them to do
things that would be difficult to do using just SVG, such as mirror a fragment
of SVG across an arbitrary axis:

![scaleAbout](https://opensolid.github.io/images/svg/1.0.2/scaleAbout.svg)
![rotateAround](https://opensolid.github.io/images/svg/1.0.2/rotateAround.svg)
![translateBy](https://opensolid.github.io/images/svg/1.0/translateBy.svg)
![mirrorAcross](https://opensolid.github.io/images/svg/1.0/mirrorAcross.svg)

Note that these functions will work on *any* `Svg msg`, value, not just ones
that happen to have been produced with this package! So you can use them as a
convenient way to transform SVG that you've produced using some other package.

## Coordinate conversion

The `relativeTo` and `placeIn` functions allow you to take SVG defined in one
coordinate system and convert it to another. For example, you can take SVG
defined in a model coordinate system where (0,0) is the center and positive Y is
up, and use `relativeTo` to convert it into SVG in window coordinates for
display, where (0,0) is the top left corner and positive Y is down.

`placeIn` is useful for 'instancing' or 'stamping' a fragment of SVG in many
different positions with different orientations:

![placeIn](https://opensolid.github.io/images/svg/1.0/placeIn.svg)

The `render2d` function provides a convenient wrapper on top of `relativeTo` to
handle both coordinate conversion and creation of the top-level SVG element.

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, use [elm-package](https://guide.elm-lang.org/install.html#elm-package)
to install `opensolid/svg`, either by running

```
elm package install opensolid/svg
```

in a command prompt inside your project directory or by adding

```json
"opensolid/svg": "3.0.0 <= v < 4.0.0"
```

to the `dependencies` field in your project's `elm-package.json`.

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/opensolid/svg/3.0.0/OpenSolid-Svg)
is available.

## Questions? Comments?

Please [open a new issue](https://github.com/opensolid/svg/issues) if you run
into a bug, if any documentation is missing/incorrect/confusing, or if there's a
new feature that you would find useful (although note that this package is not
meant to be general-purpose full-blown SVG package, more just a convenient way
to render OpenSolid geometry objects). For general questions about using this
package, try:

  - Sending me (@ianmackenzie) a message on the [Elm Slack](http://elmlang.herokuapp.com/) -
    even if you don't have any particular questions right now, just come say
    hello!
  - Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums
  - Posting to the [r/elm](https://reddit.com/r/elm) subreddit
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post OpenSolid-related stuff like demos or new releases.
Have fun, and don't be afraid to ask for help!
