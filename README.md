# keter-node

Keter Node uses the keter app as a base to spawn processes in their own working space

basically, you make your regular keter tarball, the keter-node library reads these and runs them as nodes
by renaming the binary and the executables. based on rewrite rules you supply.

Ports are transformed into port ranges

## Installation
Unlike Keter, this package is designed to be ran as a library, with an exposed API.  It does have a large footprint however,
most notably a default directory structure that is created for you in the root dir

.
├── active-nodes
├── keter-node-logs
├── node-temp
└── node-types

### node-types are the *.keter tarballs that you would normally use with keter.

### active-nodes are the locations of each of the running instances of the node-types

### keter-node-logs is the location of your LogFile information

### node-temp is where the nodes are disassembled and configured for running

## Usage

Usage is simple.


You use `initializeKeterNodeDefault` to create the directory structure in the root directory.

Then you can `spawnNewNode` with command line arguments to create a new instance of the node type,
the id is returned to you.  Which means you can `killNode` or `getNodeStatus`.





## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

TODO: Write contribution instructions here
