## Advent of Code

Welcome to Gabe's Advent of Code solutions! The `main` branch is meant to be used as a template for future years, and solutions for specific years will be in separate branches (ie `2023`).

### Installation

All installation steps will assume you are using MacOS - there should be equivalent ways to install the same software on Windows, but I'm too lazy to figure that out right now.

1. Install Homebrew (if not already installed): https://brew.sh/
2. Install sbt with `brew install sbt`
3. Install an IDE that supports Scala (I highly recommend [IntelliJ Community Edition](https://www.jetbrains.com/idea/download). Make sure to install and enable the Scala plugin)

## Usage

`sbt run` will run solutions with `Main.scala` as the entry point.

Alternatively, start an sbt console with `sbt`, then enter the `run` command. 

Keeping the sbt console running is recommended, that way it doesn't have to restart each time you want to test a change to any of the code