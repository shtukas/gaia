# Gaia

Gaia is a spin-off/re-implementation of [Nyx](https://github.com/shutkas/Nyx) with ideas coming from the proof-of-concept [Genesis](https://github.com/shutkas/Genesis).

With Gaia, not only the regular collection of Nyx objects will be available (with the features that come with them), not only will you be able to see your unmodified regular local file system hierarchy as repository, but you will also be able to browse (and partially access) files on your external hard drives while they are disconnected [1]. 

[1] Admittedly one of Pascal's key steps towards world domination.

## Why ?

Between our biological memory and the world wide web, there is a that sweet spot called "your own electronic data", mostly (but not always) located on laptops and external hard drives. Gaia (and Nyx before it) is a tool that allows its users (myself and Marcello -- at the latest count) to find things very quickly within our ever increasing large and heterogeneous collection of personal data, not like programs such as Google Desktop, but instead along the lines of our own mental associations. 

I like thinking that Nyx/Gaia are designed to make personal biological memories and personal electronic storages devices feel like a single continuous entity.

## Executables

The two executables are `gaia-utils` for overall management and introspection (see next section for details), and `gaia-web` which starts the web server.

We are using [stack](http://docs.haskellstack.org) to make Gaia. The commands below can be ran with `stack build` followed by, say, `stack exec gaia-utils general-scan`. We do not yet have a proper install script.

## gaia-utils

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
- `gaia-utils run-query <pattern>` runs a query against each FS Scan Root's Aion Tree. Returns a list of locationpaths. The results are aggregated per FS Scan Roots. 
- `gaia-utils fsck` Gaia can misbehave if the Aion tree is corrupted, this command checks that the tree is complete.  

We then have three commands to manage the list of FS Scan Roots. In this commit the scan runs at the default location, but soon the scan will run at all specified locations. The way to add, list and remove those locations is given by `gaia-utils`.

- `gaia-utils print-fs-roots`
- `gaia-utils add-fs-root <locationpath>` 
- `gaia-utils remove-fs-root <locationpath>` 

The list of FS Scan Roots, can be found in the file `~/.gaia/FSRootsListing.txt`.

## gaia-web

To run the web server do `stack exec gaia-web` (this command must be ran at the root of the source code, because the code assume that the folder "web-root" is a child of the current directory). Currently primitive, but works. In this commit queries are not yet submitted to backend, instead are echoed to the page.

## What does the search do ?

The search is currently done by `gaia-utils run-query <pattern>`. In this commit it simply returns location paths that have the pattern as substring of a foldername or filename. 

More to come in next commits...


## Todo
- **In progress**: Allow users to specify the location of their Xcache repository. (Currently hardcoded to use the existing one on Pascal's computer.)
- **Done**: To be able to scan more than one location. 
- Use the filepath package and its System.FilePath for multiplatform interfaces for file access.
- Provide a proper install script.

## Roadmap
- **In progress**: Recovering the functionalities of Gaia-Genesis. 
- **In progress**: Implementing the web server and have basic online search.
- Developping the Directives functionality.
- Reimplement all of Nyx in Gaia.
	- Connection to the sqlite database. 
	- etc.
