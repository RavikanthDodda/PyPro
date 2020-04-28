# PyPro

Programming Languages and paradigms course project

### Built using

  - MacOS
  - Linux

### Tools used

PyPro uses following languages and open source libraries:

* [SWI-Prolog] - A declarative language we used for writing parser and  runtime.
* [Python] - 
* [Sly] - A python library for doing lexical analysis and parsing.
* [Pyswip] - A python library for interfacing between python and prolog.


### Installation
Note: Currently, you can only install in linux based operating systems. But, you can still run the language in other operating systems as shown in running pypro. If you are using macOS, you may face issues while installing dependecies, but if you managed to install them correctly then you should be able to install and run the language.

Before installing make sure you have following dependencies installed correctly in your system:
* [swi-prolog] - version 8.0.3 or higher (make sure it is in your PATH)
* [python] - 3.6 or higher
* sly - `pip3 install sly`
* [pyswip]
Clicking on the links above will take you to the respective installation pages.
Note: you may need to use `pip` instead of `pip3` if you are using windows.

After you have installed all the above dependencies follow these instructions:
1. Download the project zip or clone the repo.
2. Open the terminal in the root folder of the project.
3. Execute the following commands:
```sh
$ cd src/installer/
$ sudo ./install.sh
```

### Running PyPro

You can run pypro in windows, macOs and linux operating systems without installing or building. First, make sure you have installed all the dependencies mentioned in [installation](#header2) section.

After downloading zip or cloning the repository, From project root navigate to /src/ and from command prompt:
```
$ python3 pypro.py path/filename
```
Example:
```
$ python3 pypro.py ../data/test.pr
```
Note: you may need to use `python` instead of `python3` if you are using windows.

[SWI-Prolog]: <https://www.swi-prolog.org/>
[Python]: <https://www.python.org/>
[Sly]: <https://sly.readthedocs.io/en/latest/>
[Pyswip]: https://pypi.org/project/pyswip/>

[swi-prolog]: <https://www.swi-prolog.org/download/stable>
[python]: <https://www.python.org/downloads/>
[pyswip]: <https://github.com/yuce/pyswip/blob/master/INSTALL.md>
