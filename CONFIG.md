The `dmenu` Haskell bindings support specifying default command line arguments
passed to `dmenu` in the `~/.haskell-dmenu` file.

The following shows an example `~/.haskell-dmenu` file:

    numLines         15
    font             "FiraMono:size=11"
    caseInsensitive  True
    normalBGColor    RGBColorF 0.02 0.02 0.02

Each line specifies the value of a dmenu option.
The first word of a line specifies the option, the rest of the line the value.
Depending on the option, the value has one of the following types and forms:

-   `Nat`.    A natural number, e.g. `0`, `1`, `2`, etc.
-   `Float`.  A floating point number, e.g. `-12` or `13.43`
-   `String`. A string literal, e.g. `"foo bar"`
-   `Color`.  An RGB color. For example, the color red can be specified as

    - `HexColor 0xFF0000`        (hexadecimal)
    - `RGBColor 255 0 0`         (dezimal, split components)
    - `RGBColorF 1.0 0.0 0.0`    (normalized, split components)

All `dmenu` and `dmenu2` options are supported:

-   `binaryPath              : String`
-   `displayAtBottom         : Bool`
-   `displayNoItemsIfEmpty   : Bool`
-   `grabKeyboardBeforeStdin : Bool`
-   `filterMode              : Bool`      
-   `caseInsensitive         : Bool`
-   `fuzzyMatching           : Bool`
-   `tokenMatching           : Bool`
-   `maskInputWithStar       : Bool`
-   `ignoreStdin             : Bool`
-   `spawnOnScreen           : Nat`
-   `spawnOnMonitor          : Nat`
-   `windowName              : String`
-   `windowClass             : String`
-   `windowOpacity           : Float`
-   `windowDimOpacity        : Float`
-   `windowDimColor          : Color`
-   `numLines                : Nat`
-   `heightInPixels          : Nat`
-   `underlineHeightInPixels : Nat`
-   `prompt                  : String`
-   `font                    : String`
-   `windowOffsetX           : Nat`
-   `windowOffsetY           : Nat`
-   `width                   : Nat`
-   `normalBGColor           : Color`
-   `normalFGColor           : Color`
-   `selectedBGColor         : Color`
-   `selectedFGColor         : Color`
-   `underlineColor          : Color`
-   `historyFile             : String`
-   `printVersionAndExit     : Bool`
-   `noDMenu2                : Bool`
