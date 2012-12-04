# bart

`bart` is a simple command-line program to get departure times for BART.
BART is the San Francisco Bay Area Rapid Transit.  The departure times
are retrieved via the [BART API](http://api.bart.gov/).

## Usage

Run the program by specifying the station that you want.

    $ bart 19th
    FRMT ORANGE 15, 35, 55 min
    PITT YELLOW 8, 28, 48 min
    RICH ORANGE 8, 21, 28 min
    MLBR YELLOW 13, 33, 53 min

Right now you need to pass the abbreviation of the station.  The
complete list of abbreviations can be found
[here](http://api.bart.gov/docs/overview/abbrev.aspx).
