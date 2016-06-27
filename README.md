# Reload Haskell Web IDE

This is a very basic IDE for Haskell, running as a local web server and a web front end

## Technical choices

- Server is WAI/Scotty, exposes a REST API.
- Client is Javascript/Polymer, trying to use material design components
- Uses ghcid to perform on the fly validation
- Changes to the files are persisted to disk automatically (no manual save)
- Uses the ACE web editor

## Building

- you'll need npm and bower to install the dependencies (run bower update in the web directory), since all the dependent components are NOT present in the github repo.
- the server bit can be built via stack build

## Current functionality

- Add/Delete files and folders
- Reload ghcid on file content change
- Display errors as an (ugly) menu and annotations in the editor
