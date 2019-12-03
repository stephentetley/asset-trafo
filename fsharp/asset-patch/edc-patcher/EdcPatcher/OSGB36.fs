// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace EdcPatcher




// Coordinate transformations from 
// A guide to coordinate systems in Great Britain (v2.4)
// Ordnance Survey
// Ref D00659

// OS Grid refs, see
// https://en.wikipedia.org/wiki/Ordnance_Survey_National_Grid



module OSGB36 = 

    open System.Text.RegularExpressions
    
    open AssetPatch.TemplatePatcher.Template
    open AssetPatch.TemplateCatalogue

    [<Struct>]
    /// Two letters and 10 digits
    type NGR = 
        private | NGR of string

        override x.ToString() : string = 
            let (NGR d) = x in d

        /// Pattern 2 letters then 10 chars
        static member Create(s : string) : NGR option = 
            if Regex.IsMatch(input = s, pattern = @"^([A-Za-z])([A-Za-z])([0-9]+){10}$") then 
                Some (NGR s)
            else
                None

    
    type EastingNorthing = 
        { Easting : int
          Northing : int }


   
    let private decodeMajor (ch : char) : (int * int) = 
        match ch with
            | 'S' | 's' -> (0,          0)
            | 'T' | 't' -> (500_000,    0)
            | 'N' | 'n' -> (0,          500_000)
            | 'O' | 'o' -> (500_000,    500_000)
            | 'H' | 'h' -> (0,          1_000_000)
            | _         -> (-1_000_000, -1_000_000)

    let private decodeMinor (ch : char) : (int * int) =  
        match ch with
        | 'A' | 'a' -> (0,          400_000)
        | 'B' | 'b' -> (100_000,    400_000)
        | 'C' | 'c' -> (200_000,    400_000)
        | 'D' | 'd' -> (300_000,    400_000)
        | 'E' | 'e' -> (400_000,    400_000)
        | 'F' | 'f' -> (0,          300_000)
        | 'G' | 'g' -> (100_000,    300_000)
        | 'H' | 'h' -> (200_000,    300_000)
        | 'J' | 'j' -> (300_000,    300_000)
        | 'K' | 'k' -> (400_000,    300_000)
        | 'L' | 'l' -> (0,          200_000)
        | 'M' | 'm' -> (100_000,    200_000)
        | 'N' | 'n' -> (200_000,    200_000)
        | 'O' | 'o' -> (300_000,    200_000)
        | 'P' | 'p' -> (400_000,    200_000)
        | 'Q' | 'q' -> (0,          100_000)
        | 'R' | 'r' -> (100_000,    100_000)
        | 'S' | 's' -> (200_000,    100_000)
        | 'T' | 't' -> (300_000,    100_000)
        | 'U' | 'u' -> (400_000,    100_000)
        | 'V' | 'v' -> (0,          0)
        | 'W' | 'w' -> (100_000,    0)
        | 'X' | 'x' -> (200_000,    0)
        | 'Y' | 'y' -> (300_000,    0)
        | 'Z' | 'z' -> (400_000,    0)
        | _         -> (-1_000_000, -1_000_000)


    let private decodeAlpha (s : char) (t : char) : (int * int) =  
        let (eM, nM) = decodeMajor s
        let (em, nm) = decodeMinor t
        (eM + em, nM + nm)

    // Expects an even length string
    let private readContigIntPair (ss : string) : int * int = 
        if (String.length ss) % 2 = 0 then
            let sz = String.length ss / 2
            let left = ss.[0..sz-1]
            let right = ss.[sz..(sz*2-1)]
            (int left,int right)
        else
            (0,0)

    let ngrToEastingNorthing (ngr : NGR) : EastingNorthing = 
        let major = ngr.ToString().[0]
        let minor = ngr.ToString().[1]
        let e1, n1 = readContigIntPair <| ngr.ToString().Substring(2)
        let eM, nM = decodeAlpha major minor
        { Easting = eM + e1; Northing = nM + n1 }

    let private findMajor (easting: int) (northing: int) : char =
        match (easting,northing) with
        | _ when easting >= 0 && easting < 500_000          && northing >= 0 && northing < 500_000 -> 'S'
        | _ when easting >= 500_000 && easting < 1_000_000  && northing >= 0 && northing < 500_000 -> 'T'
        | _ when easting >= 0 && easting < 500_000          && northing >= 500_000 && northing < 1_000_000 -> 'N'
        | _ when easting >= 500_000 && easting < 1_000_000  && northing >= 500_000 && northing < 1_000_000 -> 'O'
        | _ when easting >= 0 && easting < 500_000          && northing >= 1_000_000 && northing < 1_500_000 -> 'H'
        | _ when easting >= 500_000 && easting < 1_000_000  && northing >= 1_000_000 && northing < 1_500_000 -> 'J'
        | _ -> 'X'

    let private minorGrid : char[,] = 
        array2D [   [ 'V'; 'Q'; 'L'; 'F'; 'A' ];
                    [ 'W'; 'R'; 'M'; 'G'; 'B' ];
                    [ 'X'; 'S'; 'N'; 'H'; 'C' ];
                    [ 'Y'; 'T'; 'O'; 'J'; 'D' ];
                    [ 'Z'; 'U'; 'P'; 'K'; 'E' ]     ]

    let private findMinor (easting: int) (northing: int) : char =
        let modE = easting % 500000
        let modN = northing % 500000
        let divE = int (modE / 100000)
        let divN = int (modN / 100000)
        if divE >=0 && divE < 5 && divN >= 0 && divN < 5 then
            minorGrid.[divE,divN]
        else 'X'



    let eastingNorthingToNGR ({Easting = easting; Northing = northing} : EastingNorthing) : NGR =  
        let major = findMajor easting northing
        let minor = findMinor easting northing
        let smallE = easting % 100000
        let smallN = northing % 100000
        NGR <| sprintf "%c%c%05i%05i" major minor smallE smallN
    


    let east_north_ngr (ngr : NGR) : Class = 
        let ea = ngrToEastingNorthing ngr
        east_north [ easting ea.Easting ;  northing ea.Northing ]


    
