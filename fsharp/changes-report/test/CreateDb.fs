// Copyright (c) Stephen Tetley 2019

namespace SLSqlite

module CreateDb = 

    open SLFormat.CommandOptions
    open SLFormat.CommandOptions.SimpleInvoke


    let runProcess2 (workingDirectory:string option) 
                    (toolPath:string) 
                    (commandOptions:CmdOpt list)
                    (ddlFile : string) : ProcessResult = 
        try
            use proc = new System.Diagnostics.Process()
            proc.StartInfo.FileName <- toolPath
            proc.StartInfo.Arguments <- renderCmdOpts commandOptions
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardInput <- true
            proc.StartInfo.RedirectStandardOutput <- true
            proc.StartInfo.RedirectStandardError <- true
            proc.Start() |> ignore

            let content = System.IO.File.ReadAllText(ddlFile)

            proc.StandardInput.Write content

            let soReader : System.IO.StreamReader = proc.StandardOutput
            let stdout = soReader.ReadToEnd()

            let seReader : System.IO.StreamReader = proc.StandardError
            let stderr = seReader.ReadToEnd()

            proc.WaitForExit ()
            let ans : ProcessAnswer = { ExitCode = proc.ExitCode; StdOut = stdout; StdErr = stderr }
            ProcessResult.Answer ans
        with
        | excptn -> SysExn excptn


    let createDb (workingDir : string) (outputFileName : string) (ddlFilePath : string)  = 
        let arg1 = argument outputFileName
        runProcess2 (Some workingDir) "sqlite3" [arg1] ddlFilePath


    let runEchoCmd (workingDirectory:string option) 
                    (commandOptions:CmdOpt list)
                    (outfile : string) : ProcessResult = 
        try
            use proc = new System.Diagnostics.Process()
            proc.StartInfo.FileName <- "cmd"
            proc.StartInfo.Arguments <- renderCmdOpts (literal @"/c echo" :: commandOptions)
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardOutput <- true
            proc.StartInfo.RedirectStandardError <- true
            proc.Start() |> ignore


            let soReader : System.IO.StreamReader = proc.StandardOutput
            let stdout = soReader.ReadToEnd()

            System.IO.File.WriteAllText(path = outfile, contents = stdout)

            let seReader : System.IO.StreamReader = proc.StandardError
            let stderr = seReader.ReadToEnd()

            proc.WaitForExit ()
            let ans : ProcessAnswer = { ExitCode = proc.ExitCode; StdOut = stdout; StdErr = stderr }
            ProcessResult.Answer ans
        with
        | excptn -> SysExn excptn

    let runPipeToEcho (workingDirectory:string option) 
                        (textToPipe : string )
                        (outfile : string) : ProcessResult = 
        try
            use proc = new System.Diagnostics.Process()
            proc.StartInfo.FileName <- "cmd"
            proc.StartInfo.Arguments <- renderCmdOpts [literal @"/c echo"]
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardInput <- true
            //proc.StartInfo.RedirectStandardOutput <- true
            //proc.StartInfo.RedirectStandardError <- true
            proc.Start() |> ignore

          
            proc.StandardInput.WriteLine textToPipe
            proc.StandardInput.Flush ()
            proc.StandardInput.WriteLine "~~~~~~~~~"
            proc.StandardInput.Flush ()

            //let soReader : System.IO.StreamReader = proc.StandardOutput
            //let stdout = soReader.ReadToEnd()

            //System.IO.File.WriteAllText(path = outfile, contents = stdout)

            //let seReader : System.IO.StreamReader = proc.StandardError
            //let stderr = seReader.ReadToEnd()

            proc.WaitForExit ()
            let ans : ProcessAnswer = { ExitCode = proc.ExitCode; StdOut = ""; StdErr = "" }
            ProcessResult.Answer ans
        with
        | excptn -> SysExn excptn

    let runPipeDir (workingDirectory:string option)  : ProcessResult = 
        try
            use proc = new System.Diagnostics.Process()
            proc.StartInfo.FileName <- "cmd.exe"
            proc.StartInfo.Arguments <- renderCmdOpts []
            match workingDirectory with
            | Some working -> proc.StartInfo.WorkingDirectory <- working
            | None -> ()
            proc.StartInfo.UseShellExecute <- false
            proc.StartInfo.RedirectStandardInput <- true
            //proc.StartInfo.RedirectStandardOutput <- true
            //proc.StartInfo.RedirectStandardError <- true
            proc.Start() |> ignore

            proc.StandardInput.Flush()
            proc.StandardInput.WriteLine "dir"

            //let soReader : System.IO.StreamReader = proc.StandardOutput
            //let stdout = soReader.ReadToEnd()

            //System.IO.File.WriteAllText(path = outfile, contents = stdout)

            //let seReader : System.IO.StreamReader = proc.StandardError
            //let stderr = seReader.ReadToEnd()

            proc.WaitForExit ()
            let ans : ProcessAnswer = { ExitCode = proc.ExitCode; StdOut = ""; StdErr = "" }
            ProcessResult.Answer ans
        with
        | excptn -> SysExn excptn

