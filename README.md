# Gaia

Gaia is a spin-off/re-implementation of [Nyx](https://github.com/shutkas/Nyx-Ruby) in Haskell where the regular file system is the repository.

# Status

[December 2015] Gaia is a work in progress. No, really... Instrospection capabilities are avalaible but the web application is not yet worked on.

# Vocabulary

There are two repositories: The regular file system tree itself, and Xcache (at least one instance of it). Gaia only reads from the regular file system tree (from root folders that are specified by the user) and stores data in Xcache.

The data stored in Xcache are called **Aion points**. They are defined as JSON objects. Aion points are represented inside the program as **Aeson Values**. We also have the notion of **Gaia projection** of Aeson Values by which we extract from Aeson Values the data contained in the Value in a way that is more user friendly. **Filepath** is a complete FS path to a file. **Folderpath** is a complete FS path to a directory. The term used to refer to both Filepaths and Folderpaths is **Locationpath**.

# gaia-utils

`gaia-utils` can currently be used used to explore the current features of Gaia.

- `gaia-utils general-scan` runs a general scan. A general scan is defined as running a scan againt each currently known FS Scan Root. See below for managing FS Scan Roots. This command outputs each Scan Root and the corresponding Merkle Root.

```
location: /Users/pascal/Desktop
merkle  : 51154eac132a82b85efac19dccb9db1ee02fb666
location: /Galaxy/Pascal OS
merkle  : 167d207dd4a14bc619e723fd770d7c1f777f3ec3
```

- `gaia-utils get-merkle-roots` shows the value of the current Merkle roots against each FS Scan Root.
- `gaia-utils cas-get <key>` this is the `get` function of the Contents Adressable Store. Use it if you want to see the data stored against a key. (Mostly used to retrieve Aion JSON objects.) 
- `gaia-utils expose-aeson-object <key>` shows the string representation of a Aeson object. The keys are the same keys you would use for Aion objects. 
- `gaia-utils run-query <pattern>` runs a query against the Aion Tree(s). Returns a list of locationpaths.
- `gaia-utils fsck` Gaia can misbehave if the Aion tree is corrupted, this command checks that the tree is complete.  

We then have three commands to manage the list of FS Scan Roots. In this commit the scan runs at the default location, but soon the scan will run at all specified locations. The way to add, list and remove those locations is given by `gaia-utils`.

- `gaia-utils print-fs-roots`
- `gaia-utils add-fs-root <locationpath>` 
- `gaia-utils remove-fs-root <locationpath>` 

The list of FS Scan Roots, can be found in the file `~/.gaia/FSRootsListing.txt`

# Todo
- **In progress**: Allow users to specify the location of their Xcache repository. (Currently hardcoded to use the existing one on Pascal's computer.)
- **In progress**: To be able to scan more than one location. 
- Use the filepath package and its System.FilePath for multiplatform interfaces for file access

# Roadmap
- **In progress**: Recovering the functionalities of Gaia-Genesis 
- Implementing the web server and have basic online search
- Developping the Directives functionality
