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
        parser .>> spaces

    let token (str : string) : ChangeFileParser<string> = 
        lexeme (pstring str)

    let charToken (ch : char) : ChangeFileParser<char> = 
        lexeme (pchar ch)

    let directive (parser : ChangeFileParser<'a>) : ChangeFileParser<'a> = 
        charToken '*' >>. parser

    let named (name : string) 
              (parser : ChangeFileParser<'a>) : ChangeFileParser<'a> = 
        token (name + ":") >>. parser

    let cellValue : ChangeFileParser<string> = 
        manyChars (noneOf ['\t'; '\n'])


    let tab : ChangeFileParser<unit> = 
        pchar '\t' >>. preturn ()

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

    let pVariant : ChangeFileParser<unit> =
        let inner = preturn ()
        directive (named "Variant" inner)

    let pUser : ChangeFileParser<string> =
        let inner = restOfLine true |>> (fun s -> s.Trim())
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
        let line = regex ".*\|.*\|.*" |>> SelectionLine
        directive line

    let pSelectionHeader : ChangeFileParser<unit> =
        let inner = preturn ()
        directive (named "Selection" inner)

    let pSelection : ChangeFileParser<Selection list> = 
        pSelectionHeader >>. many1 (attempt pSelectionItem)

    let pHeaderRow : ChangeFileParser<HeaderRow> = 
        let inner = sepBy cellValue tab |>> (List.toArray >> HeaderRow)
        directive (inner .>> newline)


    let pDataRow : ChangeFileParser<DataRow> = 
        // Cannot use sepEndBy because we must end with tab.
        let inner = many1 (cellValue .>> tab) |>> (List.toArray >> DataRow)
        inner .>> newline

    let pDataRows : ChangeFileParser<DataRow list> = 
        many1 (attempt pDataRow)

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
            let! datas = pDataRows
            return { Header = fileHeader
                     Selection = selection
                     HeaderDescriptions = headerDescs
                     HeaderRow = headerRow
                     DataRows = datas }
        }


    let readChangeFile (inputFile : string) : Result<ChangeFile, ErrMsg> = 
        match runParserOnFile (parseChangeFile ()) () inputFile Text.Encoding.UTF8 with
        | Failure (str,_,_) -> Result.Error str
        | Success (ans,_,_) -> Result.Ok ans

