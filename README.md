# Reload Haskell Web IDE

This is a very basic IDE for Haskell, running as a local web server and a web front end.

## Technical choices

- Server is WAI/Scotty, exposes a REST API.
- Client is Javascript/Polymer, trying to use material design components
- Uses ghcid to perform on the fly validation
- Uses a WebSocket to send back validation results, etc.
- Changes to the files are persisted to disk automatically (no manual save)
- Uses the ACE web editor
- Starts the browser window automatically, closes the server when the window is closed, like Network.WAI.Launcher does (but I had to copy and modify the code since it doesn't work well if you have multiple HTML files, and of course since we're showing HTML to edit).

## Building

- You'll need npm and bower to install the dependencies (run bower update in the web directory), since all the dependent components are NOT present in the github repo.
- The server bit can be built via stack build

## Current functionality

- Add/Delete files and folders
- Reload ghcid on file content change
- Supports Cabal or Stack
- Display errors as an (ugly) menu and annotations in the editor
- Build, Test and Benchmark
- Run arbitrary commands (stack exec something, etc.)

## Configuration

Create a file called "reload.json" at the folder root. This is a JSON file. For the moment, is only supported something like:

```
{
  "editor" : {
    "theme" : "XCode"
  },
  "actions" : {
    "run1" : "stack exec echo hello"
  }
}
```

`editor.theme` is the short name for the ACE editor theme you want to use. Internally, we convert to lower case, replace spaces by underscores and prefix by ace/theme.

`actions` define some actions that will be executed in the root folder. If the key is `build` it defines the command to run when you choose *Build* in the menu (default to `cabal|stack build`). You can pass any extra options there.
If the key is `test` it defines the command to run when you choose *Build and Test* in the menu (default to `cabal|stack test`).
If the key is `bench` it defines the command to run when you choose *Build and Benchmark* in the menu (default to `cabal|stack bench`).
If the key is something else, it will appear in the list shown when you choose *Run...* in the menu, letting you specify any command you'd like to run.
