// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.Base


module QueryPatch = 
    
    open AssetPatch.Base.Common
    open AssetPatch.Base.Syntax
    open AssetPatch.Base.Typings
    open AssetPatch.Base.CompilerMonad

    type PatchQuery<'a, 'patchType> = CompilerMonad<'a, PatchFile<'patchType>>

    let runQuery (patch: PatchFile<'T>) (action : PatchQuery<'a, 'T> ) : Result<'a, ErrMsg> = 
        runCompiler patch action


    let askUser () : PatchQuery<string, 'any> = 
        asks (fun patchFile -> patchFile.Header.User)

    let askDataModel () : PatchQuery<DataModel, 'any> = 
        asks (fun patchFile -> patchFile.Header.DataModel)

    let askEntityType () : PatchQuery<EntityType, 'any> = 
        asks (fun patchFile -> patchFile.Header.EntityType)

    let askFuncLocSelectors () : PatchQuery<string list, #FuncLocSelectors> = 
        // Due to the Phantom type, extract should only see FuncLocEq's
        let extract x = 
            match x with
            | FuncLocEq str -> Some str
            | _ -> failwith "Phantom typing has made this impossible"
        asks (fun patchFile -> patchFile.Selection |> List.map extract |> List.choose id)

    let askEquiSelectors () : PatchQuery<IntegerString list, #EquiSelectors> = 
        // Due to the Phantom type, extract should only see FuncLocEq's
        let extract x = 
            match x with
            | EquiEq istr -> Some istr
            | _ -> failwith "Phantom typing has made this impossible"
        asks (fun patchFile -> patchFile.Selection |> List.map extract |> List.choose id)
    
    let askColumnHeaders () : PatchQuery<string list, 'any> = 
        asks (fun patchFile -> patchFile.HeaderRow.Columns)
