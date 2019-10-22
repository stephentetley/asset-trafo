// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base

module Parser =

    open FParsec
    open System

    open AssetPatch.Base.Syntax

    type PatchParser<'ans> = Parser<'ans, unit>

    // ************************************************************************
    // Lexer


    let intOfLength (len : int) : PatchParser<int> = 
        let after (chars : char []) = chars |> System.String |> int
        parray len digit |>> after

    let lexeme (parser : PatchParser<'a>) : PatchParser<'a> = 
        parser .>> spaces

    let token (str : string) : PatchParser<string> = 
        lexeme (pstring str)

    let charToken (ch : char) : PatchParser<char> = 
        lexeme (pchar ch)

    let directive (parser : PatchParser<'a>) : PatchParser<'a> = 
        charToken '*' >>. parser

    let named (name : string) 
              (parser : PatchParser<'a>) : PatchParser<'a> = 
        token (name + ":") >>. parser

    let cellValue : PatchParser<string> = 
        manyChars (noneOf ['\t'; '\n'])


    let tab : PatchParser<unit> = 
        pchar '\t' >>. preturn ()

    // ************************************************************************
    // Parser

    let pIntegerString : PatchParser<IntegerString> = 
        many1Chars digit |>> IntegerString |> lexeme


    let pFuncLoc : PatchParser<string> = 
        many1Chars (satisfy (not << Char.IsWhiteSpace)) |> lexeme


    let pPatchType : PatchParser<PatchType> =
        let inner = 
            choice [ token "Download" >>. preturn Download ]
        directive inner

    let pDataModel : PatchParser<DataModel> =
        let inner = 
            choice [ token "U1" >>. preturn U1 ]
        directive (named "Data Model" inner)


    let pEntityType : PatchParser<EntityType> =
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

    let pVariant : PatchParser<unit> =
        let inner = preturn ()
        directive (named "Variant" inner)

    let pUser : PatchParser<string> =
        let inner = restOfLine true |>> (fun s -> s.Trim())
        directive (named "User" inner)


    let pDate : PatchParser<int * int * int> =
        let inner = tuple3 (intOfLength 4) (intOfLength 2) (intOfLength 2)
        named "Date" (lexeme inner)
    
    let pTime : PatchParser<int * int * int> =
        let inner = tuple3 (intOfLength 2) (intOfLength 2) (intOfLength 2)
        named "Time" (lexeme inner)

    

    let pDateTime : PatchParser<DateTime> = 
        let inner = 
            parse { 
                let! (yr,mon,day) = pDate
                let! _ = charToken '/'
                let! (hr,mins,sec) = pTime
                return DateTime(year=yr, month=mon, day=day, hour=hr, 
                                minute=mins, second=sec)
            }
        directive inner
    
    
    let pSelectionId : PatchParser<SelectionId> = 
        let line1 (name : string) parser = 
           token name >>. charToken '|' >>. parser .>> charToken '|'
        
        let inner =
            choice 
                [ line1 "FUNCLOC EQ" pFuncLoc |>> FuncLocEq 
                ; line1 "EQUI EQ" pIntegerString |>> EquiEq 
                ]
        directive inner

    let pSelectionHeader : PatchParser<unit> =
        let inner = preturn ()
        directive (named "Selection" inner)

    let pSelection : PatchParser<SelectionId list> = 
        pSelectionHeader >>. many1 (attempt pSelectionId)

    let pHeaderRow : PatchParser<HeaderRow> = 
        let inner = sepBy cellValue tab |>> (List.toArray >> HeaderRow)
        directive (inner .>> newline)


    let pDataRow : PatchParser<DataRow> = 
        // Cannot use sepEndBy because we must end with tab.
        let inner = many1 (cellValue .>> tab) |>> (List.toArray >> DataRow)
        inner .>> newline

    let pDataRows : PatchParser<DataRow list> = 
        many1 (attempt pDataRow)

    let pPatchHeader : PatchParser<PatchHeader> = 
        parse {
            let! ptype = pPatchType
            let! dmodel = pDataModel
            let! etype = pEntityType
            let! variant  = pVariant
            let! user = pUser
            let! date = pDateTime
            return { PatchType = ptype
                     DataModel = dmodel
                     EntityType = etype
                     Variant = variant
                     User = user
                     DateTime = date }
        }

    let parsePatch () : PatchParser<PatchFile<'T>> = 
        parse {
            let! fileHeader = pPatchHeader
            let! selection = pSelection
            let! headerRow = pHeaderRow
            let! datas = pDataRows
            return { PatchHeader = fileHeader
                     Selection = selection
                     HeaderRow = headerRow
                     DataRows = datas }
        }


    let readPatch (inputFile : string) : Result<PatchFile<'T>, string> = 
        match runParserOnFile (parsePatch ()) () inputFile Text.Encoding.UTF8 with
        | Failure (str,_,_) -> Result.Error str
        | Success (ans,_,_) -> Result.Ok ans