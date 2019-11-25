// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Parser =

    open FParsec
    open System

    open AssetPatch.Base.Common
    open AssetPatch.Base.ChangeFile
    //    open AssetPatch.Base.AbsChangeFile

    type ChangeFileParser<'ans> = Parser<'ans, unit>

    // ************************************************************************
    // Lexer


    let intOfLength (len : int) : ChangeFileParser<int> = 
        let after (chars : char []) = chars |> System.String |> int
        parray len digit |>> after

    let lexeme (parser : ChangeFileParser<'a>) : ChangeFileParser<'a> = 
        let softSpaces = many (anyOf [' '; '\t'])
        parser .>> softSpaces

    let token (str : string) : ChangeFileParser<string> = 
        lexeme (pstring str)

    let charToken (ch : char) : ChangeFileParser<char> = 
        lexeme (pchar ch)

    let directive (parser : ChangeFileParser<'a>) : ChangeFileParser<'a> = 
        charToken '*' >>. parser .>> newline

    let named (name : string) 
              (parser : ChangeFileParser<'a>) : ChangeFileParser<'a> = 
        token (name + ":") >>. parser

    //let cellValue : ChangeFileParser<string> =
    //    manyChars (noneOf ['\t'; '\r'; '\n' ])


    // ************************************************************************
    // Parser

    let pIntegerString : ChangeFileParser<IntegerString> = 
        many1Chars digit |>> IntegerString |> lexeme


    let pFuncLoc : ChangeFileParser<string> = 
        many1Chars (satisfy (not << Char.IsWhiteSpace)) |> lexeme


    let pFileType : ChangeFileParser<FileType> =
        let inner = 
            choice [ token "Download" >>. preturn Download 
                   ; token "Upload" >>. preturn Upload
                   ]
        directive inner

    let pDataModel : ChangeFileParser<DataModel> =
        let inner = 
            choice [ token "U1" >>. preturn U1 ]
        directive (named "Data Model" inner)


    let pEntityType : ChangeFileParser<EntityType> =
        let inner = 
            choice 
                [ token "FUNCLOC"   >>. preturn FuncLoc 
                ; token "CLASSFLOC" >>. preturn ClassFloc
                ; token "VALUAFLOC" >>. preturn ValuaFloc
                ; token "EQUI"      >>. preturn Equi
                ; token "CLASSEQUI" >>. preturn ClassEqui
                ; token "VALUAEQUI" >>. preturn ValuaEqui
                ]
        directive (named "Entity Type" inner)

    let pVariant : ChangeFileParser<string> =
        let inner = restOfLine false |>> (fun s -> s.Trim())
        directive (named "Variant" inner)

    let pUser : ChangeFileParser<string> =
        let inner = restOfLine false |>> (fun s -> s.Trim())
        directive (named "User" inner)


    let pDate : ChangeFileParser<int * int * int> =
        let inner = tuple3 (intOfLength 4) (intOfLength 2) (intOfLength 2)
        named "Date" (lexeme inner)
    
    let pTime : ChangeFileParser<int * int * int> =
        let inner = tuple3 (intOfLength 2) (intOfLength 2) (intOfLength 2)
        named "Time" (lexeme inner)

    

    let pDateTime : ChangeFileParser<DateTime> = 
        let inner = 
            parse { 
                let! (yr,mon,day) = pDate
                let! _ = charToken '/'
                let! (hr,mins,sec) = pTime
                return DateTime(year=yr, month=mon, day=day, hour=hr, 
                                minute=mins, second=sec)
            }
        directive inner
    

    let pSelectionItem : ChangeFileParser<Selection> = 
        let line = regex ".*\|.*\|" |>> SelectionLine
        attempt (directive line)

    let pSelectionHeader : ChangeFileParser<unit> =
        let inner = preturn ()
        directive (named "Selection" inner)


    let pSelection : ChangeFileParser<Selection list> = 
        pSelectionHeader >>. many pSelectionItem

    let pHeaderRow : ChangeFileParser<HeaderRow> = 
        let decode (str : string) = str.Split([| '\t' |]) |> HeaderRow
        let inner = restOfLine false |>> decode
        directive inner 


    let pDataRow (size :int) : ChangeFileParser<DataRow> = 
        let decode (str : string) = 
            let arr = str.Split([| '\t' |]) 
            try 
                Array.take size arr |> DataRow |> preturn 
            with 
            | _ -> fail "bad row"
        restOfLine true >>= decode

    let pDataRows (size : int) : ChangeFileParser<DataRow list> = 
        manyTill (pDataRow size) eof

    let pFileHeader : ChangeFileParser<FileHeader> = 
        parse {
            let! ptype = pFileType
            let! dmodel = pDataModel
            let! etype = pEntityType
            let! variant  = pVariant
            let! user = pUser
            let! date = pDateTime
            return { FileType = ptype
                     DataModel = dmodel
                     EntityType = etype
                     Variant = variant
                     User = user
                     DateTime = date }
        }

    let parseChangeFile () : ChangeFileParser<ChangeFile> = 
        parse {
            let! fileHeader = pFileHeader
            let! selection = 
                match fileHeader.FileType with
                | Download -> pSelection |>> Some
                | _ -> preturn None
            let! headerDescs = 
                match fileHeader.FileType with
                | Upload -> pHeaderRow |>> Some
                | _ -> preturn None
            let! headerRow = pHeaderRow
            let size = headerRow.Columns |> List.length
            let! datas = pDataRows size
            return { Header = fileHeader
                     Selection = selection
                     HeaderDescriptions = headerDescs
                     HeaderRow = headerRow
                     DataRows = datas }
        }


    let readChangeFile (inputPath : string) : Result<ChangeFile, ErrMsg> = 
        match runParserOnFile (parseChangeFile ()) () inputPath Text.Encoding.UTF8 with
        | Failure (str,_,_) -> Result.Error str
        | Success (ans,_,_) -> Result.Ok ans

