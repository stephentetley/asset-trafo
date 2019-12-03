// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetPatch.TemplateCatalogue


[<AutoOpen>]
module AssetCondition =
    
    open AssetPatch.TemplatePatcher.Template

    
    

    /// ASSET_CONDITION
    let asset_condition : Characteristic list -> Class = 
        _class "ASSET_CONDITION" 1013u

    // ASSET_CONDITION:ConditionGrade
    type ConditionGrade = 
        | Good 

        override x.ToString() = 
            match x with
            | Good -> "1 - GOOD"

        static member TryParse (source : string) : ConditionGrade option = 
            match source.ToUpper() with
            | "GOOD" -> Some Good 
            | _ -> None

            
    /// ASSET_CONDITION:CONDITION_GRADE
    let condition_grade (v : ConditionGrade) : Characteristic = 
        _characteristic "CONDITION_GRADE" (v.ToString())


    /// ASSET_CONDITION:CONDITION_GRADE_REASON
    let condition_grade_reason (v : string) : Characteristic = 
        _characteristic "CONDITION_GRADE_REASON" (v.ToUpper())

    /// ASSET_CONDITION:PerformanceGrade
    type PerformanceGrade = 
        | Availability_95 

        override x.ToString() = 
            match x with
            | Availability_95 -> "1 - AVAILABILITY 95%"

        static member TryParse (source : string) : PerformanceGrade option = 
            match source.ToUpper() with
            | "95%" -> Some Availability_95 
            | _ -> None


    /// ASSET_CONDITION:PERFORMANCE_GRADE
    let performance_grade (v : PerformanceGrade) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE" (v.ToString())


    /// ASSET_CONDITION:PERFORMANCE_GRADE_REASON
    let performance_grade_reason (v : string) : Characteristic = 
        _characteristic "PERFORMANCE_GRADE_REASON" (v.ToUpper())


    /// ASSET_CONDITION:LoadingFactor
    type LoadingFactor = 
        | Satisfactory 

        override x.ToString() = 
            match x with
            | Satisfactory -> "3 - SATISFACTORY"

        static member TryParse (source : string) : LoadingFactor option = 
            match source.ToUpper() with
            | "SATISFACTORY" -> Some Satisfactory 
            | _ -> None

    /// ASSET_CONDITION:LOADING_FACTOR
    let loading_factor (v : LoadingFactor) : Characteristic = 
        _characteristic "LOADING_FACTOR" (v.ToString())


    /// ASSET_CONDITION:LOADING_FACTOR_REASON
    let loading_factor_reason (v : string) : Characteristic = 
         _characteristic "LOADING_FACTOR_REASON" (v.ToUpper())


    /// ASSET_CONDITION:SURVEY_DATE
    let survey_date (year : uint32) : Characteristic = 
        _characteristic "SURVEY_DATE" (year.ToString())
