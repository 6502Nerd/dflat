//========================================================================================
// BeebVSC
// Visual Studio Code Extension to support 6502/BBC Micro development
// This script provides functionality to manage build targets via VSC tasks
//----------------------------------------------------------------------------------------
//  Author: simondotm (https://github.com/simondotm)
//  GitHub: https://github.com/simondotm/beeb-vsc
// License: MIT
//----------------------------------------------------------------------------------------
// Notes:
// I'm using plain old JavaScript because TS seemed a faff to setup/compile.
// In principle this script could be generalised to use different assemblers, but I thought
//  it was best to focus on BeebAsm initially.
// Only supports Windows for now (since I'm a windows user) as I'm not sure how much
//  appetite there is for Mac/Linux support.
//----------------------------------------------------------------------------------------
// Thanks to the following projects for providing inspiration:
// https://github.com/mirao/mads
// https://github.com/alefragnani/vscode-project-manager
//========================================================================================

// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
var vscode = require('vscode');
var child_process = require('child_process');
var fs = require('fs');

//----------------------------------------------------------------------------------------
// the list of source filename extensions we recognize as assembler/source files
//----------------------------------------------------------------------------------------
var supportedFileTypes = [ "asm", "6502", "s" ];

//----------------------------------------------------------------------------------------
// the default filename extension of target filenames 
// these are only applied when targets are first created.
// user can manually override this in the tasks.json file (eg. if .DSD preferred)
//----------------------------------------------------------------------------------------
var targetExt = ".ssd";

//----------------------------------------------------------------------------------------
// the default ProblemMatcher for BeebAsm assembler output
// ProblemMatcher's are used to parse output from compilers etc. to capture warnings/errors within the IDE
// BeebAsm output format is <filename>:<line>: <errortype>: <error details>
// BeebAsm doesn't indicate column of the error, but BeebAsm usually emits the offending line with a '^' underneath pointing to the problem area
//----------------------------------------------------------------------------------------
var problemMatcher = {
    "owner": "6502",
    "fileLocation": ["relative", "${workspaceRoot}"],
    "pattern": {
        "regexp": "^(.*):(\\d+):\\s+(warning|error):\\s+(.*)$",
        "file": 1,
        "line": 2,
        "severity": 3,
        "message": 4
    }
};


//----------------------------------------------------------------------------------------
// The default tasks.json file template
// We use "cmd" as a shell command, so that tasks can be invoked with any executable command
//----------------------------------------------------------------------------------------
var tasksHeader = {
    "version": "0.1.0",
    "command": "cmd",
    "isShellCommand": true,
    "showOutput": "always",
    "echoCommand": true,
    "suppressTaskName": true,
    "args" : ["/C"],
    "tasks": []    
};


// We could (in principle) support other plaforms in the tasks.json file as follows:
// Windows   "command": "cmd",    "args": ["/C"],   
// Linux     "command": "sh",    "args": ["-c"],
// TODO: support linux/mac? https://code.visualstudio.com/docs/editor/tasks#_operating-system-specific-properties


//----------------------------------------------------------------------------------------
// returns the current user/workspace VSC configuration for the BeebVSC assembler preference
//----------------------------------------------------------------------------------------
function getAssemblerPath()
{
    var assemblerPath = vscode.workspace.getConfiguration('beebvsc').get('assembler', 'Name');
    return assemblerPath;         
}

//----------------------------------------------------------------------------------------
// returns the current user/workspace VSC configuration for the BeebVSC emulator preference
//----------------------------------------------------------------------------------------
function getEmulatorPath()
{
    var emulatorPath = vscode.workspace.getConfiguration('beebvsc').get('emulator', 'Name');
    return emulatorPath;         
}


//----------------------------------------------------------------------------------------
// returns the full path to the .vscode directory in the current workspace
//----------------------------------------------------------------------------------------
function getVSCodePath()
{
    var vscodePath = vscode.workspace.rootPath + "/.vscode";
    return vscodePath;    
}

//----------------------------------------------------------------------------------------
// returns the full path to the tasks.json file in the current workspace
//----------------------------------------------------------------------------------------
function getTasksPath()
{
    var tasksPath = getVSCodePath() + "/tasks.json";
    return tasksPath;
}

//----------------------------------------------------------------------------------------
// given a filename, returns the equivalent target file (ie. replaces extension with ".ssd") 
//----------------------------------------------------------------------------------------
function getTargetName(source)
{
    var ext = source.lastIndexOf(".");
    if (ext < 0)
        return source + targetExt;
    else
        return source.substring(0, ext) + targetExt;
}

//----------------------------------------------------------------------------------------
// check if the given file exists and returns true or false
//----------------------------------------------------------------------------------------
function fileExists(path)
{
    // fs.existsSync is deprecated (https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
    try {
        var stats = fs.statSync(path);
        if (stats.isFile())
            return true;
    }
    catch (err)
    {
        if (err && err.code === 'ENOENT') {
            // file doesn't exist
        } else if (err) {
        }       
    }
    return false;
}

//----------------------------------------------------------------------------------------
// check if the given directory exists and returns true or false
//----------------------------------------------------------------------------------------
function dirExists(path)
{
    // fs.existsSync is deprecated (https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
    try {
        var stats = fs.statSync(path);
        if (stats.isDirectory())
            return true;
        // is a file, so return false
    }
    catch (err)
    {
        if (err && err.code === 'ENOENT') {
            // dir doesn't exist
        } else if (err) {
        }       
    }
    return false;
}




//----------------------------------------------------------------------------------------
// save the given JSON object as "{workspace path}/.vscode/tasks.json"
// returns true if successful or false if failure
// if no .vscode directory exists, it will be created.
//----------------------------------------------------------------------------------------
function saveTasks(tasksObject)
{
    var vscodePath = getVSCodePath();
 
    // sanity check that .vscode is not a file. 
    if (fileExists(vscodePath))
    {
        vscode.window.showErrorMessage("BeebVSC: '.vscode' exists as a file rather than a directory! Unexpected - Please resolve.");
        return false;
    }

    // create .vscode directory if it does not already exist
    if (!dirExists(vscodePath)) {
        fs.mkdirSync(vscodePath);
    }

    // now we can write the tasks.json file
    var tasksPath = getTasksPath();
    try {
        var output = JSON.stringify(tasksObject, null, 4);
        tasksFile = fs.writeFileSync(tasksPath, output, 'utf8');
    }
    catch(err) {
        vscode.window.showErrorMessage("BeebVSC '" + err + "', when saving file 'tasks.json'");    
        console.log("error saving tasks.json '" + err + "'");  
        return false;   
    }
    console.log("saved tasks.json");    
    return true;
}

//----------------------------------------------------------------------------------------
// load the tasks.json file from the local workspace
// returns the file as a JS object or null if there was an error
// if no tasks.json file exists, a default will be created.
//----------------------------------------------------------------------------------------
function loadTasks()
{
    if (!checkConfiguration())
    {
        return null;    
    }

    // if no tasks.json file exists, create a default
    var tasksPath = getTasksPath();
    if (!fileExists(tasksPath))
    {
        var tasksObject = tasksHeader;
        // if tasks file could not be saved, return null to indicate problem.
        if (!saveTasks(tasksObject)) 
            return null;
    }


    // load the tasks.json file
    var tasksFile = null;
    try {
        tasksFile = fs.readFileSync(tasksPath, 'utf8');
    }
    catch(err) {
        vscode.window.showErrorMessage("Could not load tasks file");    
        return null;   
    }
      
    // loaded, so parse as JS object
    var tasksObject = JSON.parse( tasksFile );
    console.log("loaded tasks.json");

    // sanity check - ensure a tasks array exists if not present
    if (!("tasks" in tasksObject)) {
        tasksObject["tasks"] = [];
        console.log("Added tasks array to tasks.json");
    }

    return tasksObject;
}


//----------------------------------------------------------------------------------------
// check pre-requisites for a build-related command
// returns true if configuration is ok or false otherwise.
//----------------------------------------------------------------------------------------
function checkConfiguration()
{
    // First check that the user has configured assembler & emulator settings 
    var assemblerPath = getAssemblerPath();
    var emulatorPath = getEmulatorPath();

    if (assemblerPath == "" || emulatorPath == "")
    {
        // config is not set properly, prompt user to set these first then return
        vscode.window.showErrorMessage("BeebVSC global assembler/emulator configurations not set. Set them now?", "Yes").then((item) => {
                                        if (item === "Yes") {
                                            // show settings
                                            vscode.commands.executeCommand("workbench.action.openGlobalSettings");
                                            vscode.window.showInformationMessage("Add the 'beebvsc.assembler' and 'beebvsc.emulation' properties to your user or workspace settings file.");
                                        }
                                    });    

        console.log("checkConfiguration failed - missing configuration");                                           
        return false; 
    }

    // TODO: check that the configured assembler & emulator can be 'reached', via path environment var or directly.

    console.log("checkConfiguration passed");
    return true;
}

//----------------------------------------------------------------------------------------
// sets the build target with the given named 'target' as the default build command
// also sets the test target to run the given named 'target'
//----------------------------------------------------------------------------------------
function setCurrentTarget(tasksObject, target)
{
    var tasks = tasksObject["tasks"];
    // first find the target and set it as the default build
    for (var i = 0; i < tasks.length; ++i) {
        var task = tasks[i];
        if (!("isTestCommand" in task)) {
            // add isBuildCommand to target task
            if (task["taskName"] === target) {
                task["isBuildCommand"] = true;
            }else
            {
                // remove isBuildCommand from all other tasks
                if ("isBuildCommand" in task) {
                    delete task["isBuildCommand"];
                }
            }
        }
 
    }

    // next, find the test task and update it to run the new target
    var task = null;
    for (var i = 0; i < tasks.length; ++i) {
        if ("isTestCommand" in tasks[i]) {
            task = tasks[i];
        }        
    }    

    // if no test task was found, create one.
    if (task == null) {
        task = {};
        task["taskName"] = "";
        task["isTestCommand"] = true;
        task["args"] = [""];
        tasks.push(task);

        // run the search again, it will find the new test task we just created.
        for (var i = 0; i < tasks.length; ++i) {
            if ("isTestCommand" in tasks[i]) {
                task = tasks[i];
            }        
        }         
   
    }

    // Set Emulator/BeebEm command line arguments as the test command
    var arg = "";
    arg += getEmulatorPath();  // assembler command      
    arg += " ";
    arg += target;
    task["args"] = [ arg ];    
    // update test task name also
    task["taskName"] = "Run '" + target + "' in Emulator";    
}


//----------------------------------------------------------------------------------------
// create a new build target within the tasks.json configuration
// if no tasks.json file already exists, one will be created
// otherwise the new target will be added to the tasks list
// we also check for and prevent duplicate targets
// the user is prompted to select a source file from the workspace as the new build target
//----------------------------------------------------------------------------------------
function createTargetCommand()
{
    // to create a new target some pre-requisites are needed
    // assembler configuration must be set (user or workspace) - prompt to set if not
    // emulator configuration must be set (user or workspace) - prompt to set if not
    if (!checkConfiguration())
        return;

    var targetList = [];
    var rootPath = vscode.workspace.rootPath;
    var fileNames = fs.readdirSync(rootPath);
    for (var i = 0; i < fileNames.length; i++) {
        var path = rootPath + "/" + fileNames[i];
        try {
            var stats = fs.statSync(path);
            if (stats.isFile())
            {
                for (var j = 0; j < supportedFileTypes.length; ++j)
                {
                    var ext = "." + supportedFileTypes[j];
                    if (path.toLowerCase().endsWith(ext))
                    {
                        console.log("found source file '" + fileNames[i] + "'");
                        targetList.push(fileNames[i]);
                        break;
                    }
                }

            }
        }
        catch (err)
        {
            vscode.window.showErrorMessage("BeebVSC '" + err + "'");
        }
    }

    // show popup list picker in VSC for user to select desired new target file
    var pickOptions = { "ignoreFocusOut" : true, "placeHolder" : "Create New Build Target: Select a source file" };
    vscode.window.showQuickPick(targetList, pickOptions).then(selection => {
        // if focus lost, or user cancels
        if (typeof selection == 'undefined') {
            console.log("bad selection");
            return;
        }			
        console.log("selected '" + selection + "'");

        // now we load the tasks.json
        var tasksObject = loadTasks();
        // sanity check - cant see how this might happen unless running on wierd platform or with wierd file permissions
        if (tasksObject == null)
        {
            vscode.window.showErrorMessage("BeebVSC 'Error loading tasks.json' - contact developer");
            return;            
        }



        // Generate the target(output) filename
        var target = getTargetName(selection);

        // get the tasks array, we will add our new target here.
        var tasks = tasksObject["tasks"];

        // Check if this target is already present, we dont handle this presently
        // TODO: offer option to replace
        for (var i = 0; i < tasks.length; ++i){
            if (target.toLowerCase() === tasks[i]["taskName"].toLowerCase())
            {
                vscode.window.showErrorMessage("BeebVSC - Can't add target '" + target + "' as it already exists.");       
                return;   
            }                

        }

        // Create the new build target
        var task = {};
        task["taskName"] = target;
        task["problemMatcher"] = problemMatcher;


        // create BeebAsm commandline arguments - note there's only one argument since we're using cmd as the main shell command in the tasks.json
        // https://github.com/tom-seddon/beebasm/blob/master/about.txt
        
        var arg = "";
        arg += getAssemblerPath();  // assembler command      
        arg += " -v";               // verbose
        arg += " -i " + selection;  // source file to assemble
        arg += " -do " + target;    // target file to output
        arg += " -boot Main";       // bootable target, *RUN Main
        task["args"] = [ arg ];

        // add the new task
        tasks.push(task);

       // set as the default build & test target if this is the first task
        // TODO: check if other targets already exist, but not are set as the default build? Can do this with target select.
        if (tasks.length == 1)
        {
            setCurrentTarget(tasksObject, target);
        }


        // write the new tasks.json file
        if (saveTasks(tasksObject)) {
            vscode.window.showInformationMessage("BeebVSC - added new build target '" + target + "'");
        }else
        {
            vscode.window.showErrorMessage("BeebVSC - Error updating tasks.json, build target '" + target + "' could not be created.");
        }





    });


    // .vscode folder present - create if not
    // BeebVSC compatible tasks.json file present (with empty task list possibly) - create or overwrite if not



    // Present List of potential target files (*.6502, *.asm, *.s)
    // User selects source file
    // Create new task object for this source file
    // If only one task object exists, or no other defaults, select this as the default
    // If no isTestCommand object exists, create one using this target
    // taskname is the source file name (without file extension?)

    // synchronize task args with current assembler & emulator configs
    // write updated tasks.json
}

//----------------------------------------------------------------------------------------
// present the user with a list of current build targets in the tasks.json file
// any selection will update the tasks.json file to have the new target as the default
// build target.
// the tests tasks will also be updated to reflect the new build target.
//----------------------------------------------------------------------------------------
function selectTargetCommand()
{
    // to select a default target some pre-requisites are needed
    // assembler configuration must be set (user or workspace) - prompt to set if not
    // emulator configuration must be set (user or workspace) - prompt to set if not
    if (!checkConfiguration())
        return;    
        
    // Plus must have at least one target in the list of tasks
    // now we load the tasks.json
    var tasksObject = loadTasks();
    // sanity check - cant see how this might happen unless running on wierd platform or with wierd file permissions
    if (tasksObject == null)
    {
        vscode.window.showErrorMessage("BeebVSC 'Error loading tasks.json', could not select new target");
        return;            
    }

    var targetList = [];    
    var tasks = tasksObject["tasks"];   
    if (tasks.length == 0)
    {
        vscode.window.showErrorMessage("BeebVSC 'No targets available in tasks.json'");
        return;             
    }

    for (var i = 0; i < tasks.length; ++i)
    {
        var task = tasks[i];
        if (!("isTestCommand" in task)){
            targetList.push(task["taskName"]);
        }
    }

    // show popup list picker in VSC for user to select desired new target file
    var pickOptions = { "ignoreFocusOut" : true, "placeHolder" : "Select Build Target: Choose a new default target file" };
    vscode.window.showQuickPick(targetList, pickOptions).then(selection => {
        // if focus lost, or user cancels
        if (typeof selection == 'undefined') {
            console.log("bad selection");
            return;
        }			
        console.log("selected '" + selection + "'");

        // set the selection as the new default build target + test command
        setCurrentTarget(tasksObject, selection);

  
        // write the new tasks.json file
        if (saveTasks(tasksObject)){
            vscode.window.showInformationMessage("BeebVSC - selected new build target '" + selection + "'");
        } else {
            vscode.window.showErrorMessage("BeebVSC Error updating tasks.json - build target  '" + selection + "' could not be selected");
        }

      


    });



}



//----------------------------------------------------------------------------------------
// Execute the given command
// Was previously used to run the emulator, but that's now handled via tasks.json
//----------------------------------------------------------------------------------------
function execCommand(command, args)
{
    console.log('execCommand "' + command + '", args "' + args + '"');
    let proc = child_process.spawn(command, args, { stdio: 'pipe' });
    let stderr = '';
    let stdout = '';

    proc.stderr.on('data', (data) => {
        console.log('captured stderr');
        stderr += data.toString();
    });
    proc.stdout.on('data', (data) => {
        console.log('captured stdout');
        stdout += data.toString();
        console.log(data.toString());
    });


    proc.on('error', (err) => {
        console.log("error '" + err + "'");
        vscode.window.showErrorMessage("Could not launch '" + command + "', args '" + args + "'");
    });

   proc.on('close', (code) => {
        if (code !== 0) {
            console.log("Error running '" + command + "', args '" + args + "'");
            vscode.window.showErrorMessage("Could not launch '" + command + "', args '" + args + "'");
        }

    });

  /*
    proc.stderr.on('data', (data: Buffer) => {
        stderr += data.toString();
    });
    proc.stdout.on('data', (data: Buffer) => {
        stdout += data.toString();
    });
    proc.on('error', (err: Error) => {

    });
    proc.on('close', (code: number) => {
        if (code !== 0) {
      //      console.error(stderr);
      //      console.error(stdout);
      //      console.error(`Error running '${command} ${args.join(' ')}'`);
        }
    });
*/

    console.log("command executed ok");
    console.log('stderr "' + stderr + '"');
    console.log('stdout "' + stdout + '"');


    return true;
}



//----------------------------------------------------------------------------------------
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
//----------------------------------------------------------------------------------------
function activate(context) {

    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('BeebVSC extension activated!');
    console.log("path " + vscode.workspace.rootPath);
//    vscode.window.showInformationMessage('BeebVSC extension activated.');

    // The command has been defined in the package.json file
    // Now provide the implementation of the command with  registerCommand
    // The commandId parameter must match the command field in package.json

    // Create a new build target
    var disposable = vscode.commands.registerCommand('extension.target.create', function () {
        createTargetCommand();
    });

    // Set a new default build target
    var disposable = vscode.commands.registerCommand('extension.target.select', function () {
        selectTargetCommand();
    });    

    // Run (Test) the current build target - just a wrapper for the "Run test task"
    var disposable = vscode.commands.registerCommand('extension.target.run', function () {
        vscode.commands.executeCommand("workbench.action.tasks.test");
    });      

    // Build the current target - just a wrapper for the "Run build task"
    var disposable = vscode.commands.registerCommand('extension.target.build', function () {
        vscode.commands.executeCommand("workbench.action.tasks.build");
    });            

/*
    // TODO: remove?
    var disposable = vscode.commands.registerCommand('extension.run', function () {
        // The code you place here will be executed every time your command is executed

        // Display a message box to the user
        vscode.window.showInformationMessage('Launching Emulator...');


        var beebemulator = vscode.workspace.getConfiguration('beebvsc').get('emulator', 'Name');
        console.log("emulator path " + beebemulator);
        var target = vscode.workspace.rootPath + "/sampleplayer.ssd"
        args = [ target ];
        execCommand(beebemulator, args);     
    });      
*/

    context.subscriptions.push(disposable);
}
exports.activate = activate;

// this method is called when your extension is deactivated
function deactivate() {
}
exports.deactivate = deactivate;