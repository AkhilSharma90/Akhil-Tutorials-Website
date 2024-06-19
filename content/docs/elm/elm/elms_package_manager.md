---
title: "Package Manager in Elm"
description: "Handling HTTP requests and JSON data in Elm."
icon: "code"
draft: false
---

A package manager is a command-line tool that automates the process of installing, upgrading, configuring, and removing packages in your application.

Just like JavaScript has a package manager called npm, elm has a package manager called elm-package.

The package manager performs the following three tasks −

- Installs all dependencies that an elm application need
- Publishes custom packages
- Determines the version of your package when you are ready to publish and update.

## Elm Package Manager Commands
The following table lists down the various Elm package manager commands −

Sr. No.	Command	Syntax	Description
1.	install, elm-package install	Installs packages to use locally

2.	publish	elm-package publish	Publishes your package to the central catalog

3.	bump	elm-package bump	Bumps version numbers based on API changes

4.	diff	elm-package diff	Gets differences between two APIs
In order to publish your package, you need to host source code on GitHub and have the version properly labeled with a git tag. Following illustration shows how to use elm-package manager to pull an external dependency.

Illustration - Installing svg package
In this example, we will see how to integrate Scalable Vector Graphics(SVG) into an elm application.

Step 1 − Create a folder elmSvgApp

Step 2 − Install svg package using the following command −

```sh
elm-package install elm-lang/svg
```

Step 3 − Install Create a SvgDemo.elm file and type the content given below. We import Svg module to draw a rectangle of 100x100 dimension and fill the colour red.

```elm
import Svg exposing (..)
import Svg.Attributes exposing (..)

main =
   svg
   [ width "120"
   , height "120"
   , viewBox "0 0 120 120"
   ]
   [ rect
      [ x "10"
      , y "10"
      , width "100"
      , height "100"
      , rx "15"
      , ry "15"
      ,fill "red"
      ]
      []
   ]
```

Step 4 − Now build the project using elm make .\SvgDemo.elm. This will generate an index.html and you can open it on your browser.

### Learn How To Build AI Projects

Now, if you are interested in upskilling in 2024 with AI development, check out this 6 AI advanced projects with Go where you learng about building with AI and getting the best knowledge there is currently. Here's the [link](https://akhilsharmatech.gumroad.com/l/zgxqq).
