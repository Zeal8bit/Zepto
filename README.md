<h2 align="center">Zepto text editor</h2>
<p align="center">
    <img src="imgs/screen.png" alt="Screenshot" />
</p>
<p align="center">
    <a href="https://opensource.org/licenses/Apache-2.0">
        <img src="https://img.shields.io/badge/License-Apache_2.0-blue.svg" alt="Licence" />
    </a>
    <p align="center">Presenting <b>Zepto</b>: A clone of nano entirely written in Z80 assembly, for Zeal 8-bit OS</p>
</p>


## About Zepto

The goal of Zepto is to provide a text editor for [Zeal 8-bit OS](https://github.com/Zeal8bit/Zeal-8-bit-OS) that is complete enough to write some code of text files, without taking too much disk space.
This is why I decided to make a clone of the famous `nano` text editor present on Linux. The problem is `nano` is written in C and is tied to the libC and Linux kernel. As such, I decided to write a clone from scratch, in Z80 assembly, to minimize the space and time it will take.

Thus, `nano` was only used as a source of **inspiration**, its code was not used **at all** in this project.

## Features and limitations

Zepto is still in alpha, here are the features implemented:

* Less than **4KB** once compiled!
* Use Zeal 8-bit OS cursor and keyboard API, so completely portable
* Use *Gap Buffer* data structure to avoid line length limitation
* Use Controller-Model-View architecture
* Open an existing file given as a parameter
* Edit the file content, can split lines, can add characters at the beginning, at the middle and at the end of lines
* Support upper, lower case and special characters
* Support scrolling up and down in the file
* Support `Ctrl + Key` for menu selection
* Support `Location` option
* Support `Save file` option
* Support `Exit` option
* Support "Save file on exit" question

Regarding the current limitations:

* Screen size is hardcoded to 80x40 characters. In the future, it should use Zeal 8-bit OS video API to get the screen area size
* Doesn't support memory paging, so the maximum file size is limited by the amount of RAM mapped at once: around 40KB
* No way to remove an empty line. Once a new line is created, it cannot be removed, it'll always be in the file
* It is not possible to modify the file destination once the file is opened. For example, if the file `B:/file.txt` is opened, it is not possible to save it under a new name
* Copy/Paste/Cut/Help/Open File/New File are not implemented

## Building the project

### Requirements

The recommended build environment is [Zeal Development Environment (ZDE)](https://github.com/Zeal8bit/zeal-dev-environment), which provides the Zeal CMake toolchain and z88dk tools.

For a native build, install:

* Git (also used to embed the current revision in the title bar)
* CMake 3.16 or later and a supported build tool, such as Make or Ninja
* A current Zeal 8-bit OS checkout containing `cmake/` and `sdk/z88dk-z80asm/`
* Zeal 8-bit OS v0.3.0 or above on the target computer. The older versions don't implement cursor and video APIs
* z88dk v2.2 (or later), including `z88dk-z80asm` and `z88dk-dis`. The assembler must support `MACRO`.

To install z88dk, please [check out their Github project](https://github.com/z88dk/z88dk).

### Building

From the project directory, configure and build with ZDE:
```sh
zde cmake
```

For a native build, set the path to Zeal 8-bit OS and run CMake:
```sh
export ZOS_PATH=/your/path/to/Zeal8bitOS
cmake -S . -B build
cmake --build build
```

The project selects Zeal's `z88dk` ASM toolchain, which supplies the SDK header path. The deprecated project Makefile and `zde make` are no longer used.

After compiling, `bin/` contains `zepto.bin`, its symbol map (`zepto.map`), and disassembly (`zepto.dump`). Objects and the generated `version.txt` stay in `build/`. The binary retains its load address of `0x4000` and can be loaded through UART using the `load` command.

To clean the build outputs:
```sh
zde cmake build --target clean
```

The binary can also be embedded in an OS romdisk. With ZDE, stage it using `zde image romdisk add bin/zepto.bin`, then follow the [ZDE image documentation](https://github.com/Zeal8bit/zeal-dev-environment/blob/main/docs/image.md) to build the image.

More info about compiling Zeal 8-bit OS [here](https://github.com/Zeal8bit/Zeal-8-bit-OS#getting-started).

The resulted ROM image can then be provided to an emulator or directly flashed to the computer's ROM that will use it.

## Usage

If you are using Zeal 8-bit OS default romdisk, you can use `exec` command to execute `zepto.bin` or directly use `./`, followed by the source/destination file:
```
A:/>exec zepto.bin B:/your_file.txt
```
or
```
A:/>./zepto.bin B:/your_file.txt
```

As said above, keep in mind that once opened, the file cannot be renamed or saved somewhere else, so make sure you specify a destination disk that is **write-read**! (`A:/` disk isn't by default)


## Contact

For any suggestion or request, you can contact me at contact [at] zeal8bit [dot] com

Or, you can join Zeal 8-bit projects [Discord server](https://discord.gg/UzEjwRvBBb).

For features requests, you can also open an issue or a pull request.

## Contributing

Contributions are welcome! Feel free to fix any bug that you may see or encounter, or implement any feature that you find important.

To contribute:
  * Fork the Project
  * Create your feature Branch (*optional*)
  * Commit your changes. Please make a clear and concise commit message (*)
  * Push to the branch
  * Open a Pull Request


(*) A good commit message is as follow:
```
Module: add/fix/remove a from b

Explanation on what/how/why
```
For example:
```
Flash: add the possibility to flash programs bigger than 48KB
```

## License

Distributed under the Apache 2.0 License. See `LICENSE` file for more information.

You are free to use it for personal and commercial use, the boilerplate present in each file must not be removed.
