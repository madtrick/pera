[![Analytics](https://ga-beacon.appspot.com/UA-46795389-1/pera/README)](https://github.com/igrigorik/ga-beacon)


#PERA
Pure Erlang RESTful API to monitor the Erlang runtime.

* [About](#about)
* [Examples](#examples)
* [Resources](#resources)
* [Usage](#usage)
* [Tests](#tests)
* [Contributing](#contributing)
* [Author](#author)
* [License](#license)


## About <a name="about"></a>

PERA is a RESTful API that exposes as resources info about a running Erlang VM. This resources provide a simple way to access information about the Erlang runtime.

This API can be exploited to build more powerful tools like webapps or command line tools that give a simple and more focused view of the current state of the runtime. 

## Examples <a name="examples"></a>

Below is a request to GET the ```/processes/:pid``` resource:

![process resource](https://raw.github.com/madtrick/pera/readme-assets/readme-assets/resource_process.png)

## Resources  <a name="resources"></a>
The resources group info about the Erlang runtime in way that is easy an useful to consume. Most of the info is fetched using functions in the ```erlang``` module.

### Available resources

The exposed resources are:

* ```/``` : root of the API. Contains links to the other exposed resources.
* ```/processes``` : collection of all running processes.
* ```/processes/:pid``` : a single running process.
* ```/modules``` : collection of all the loaded modules.
* ```/memory``` : memory usage in the runtime.

More resources will be added as the API is developed.

### Media type
PERA uses [HAL](http://stateless.co/hal_specification.html)+json as the media type for representing its resources and their relations with hyperlinks. This has the following benefits:

* The resources share a simple but standarized format.
* Documentation can be discovered through the API.
* Relations can be discovered through the API.
* It is not reinventing the wheel. This means that there are already [tools](https://github.com/mikekelly/hal_specification/wiki/Libraries) out there to consume APIs that use HAL as media type.
* Its human and machine readable.

### Relations documentation

Location of the documentation for the relations accessible from a resource is given in the own resource. As the HAL spec says:

> Link rels should be URLs which reveal documentation about the given link


```json
    "_links": {
        "curies": {
            "href": "/relations/{rel}",
            "name": "pera",
            "templated": true
        },
        "pera:module": {
            "href": "/modules/{module}",
            "templated": true
        },
        "pera:modules": {
            "href": "/modules"
        },
```

For example, the previous resource points out that you could find info about ```pera:module``` relation at ```/relations/module``` or for ```pera:modules``` at ```/relations/modules```.

### HAL browser
![hal browser](https://raw.github.com/madtrick/pera/readme-assets/readme-assets/hal-browser.png)

The [HAL browser](https://github.com/mikekelly/hal-browser) is included in the aplication to help with the usage of the API and to highlight the benefits of auto-discoverable resources. To use it, point your browser to [http://localhost:8000/hal-browser/](http://localhost:8000/hal-browser/) and navigate through the available resources and documentation.



## Usage  <a name="usage"></a>
Start the PERA application and the API will be available at ```0.0.0.0:8000```.


## TODO  <a name="todo"></a>

* Add more resources. For example:
	* ```/modules/:module```: info about a module.
	* ```/system``` : general info about the runtime.
	* ```/system/allocators``` : info about the current allocators.
	* ```/system/cpu``` : info about de CPU topology.
* Allow parameterized requests so the response contain only the neccesary info.
* Include a configuration file to easily setup PERA i.e. port, address, etc.

## Tests <a name="tests"></a>
Unit test where done with the Eunit. To run them:

  ```
  rake test
  ```
or, in case you don't have rake installed:

  ```
  rebar eunit skip_deps=true
  ```

## Contribute <a name="contributing"></a>

Ideas are allways welcomed and if you find or think that something isn't working properly, just open an issue.

Pull requests and patches are welcome.

## Author <a name="author"></a>

This stuff has been writen by Farruco sanjurjo

  * [@madtrick](https://twitter.com/madtrick) at twitter
  * Email at [madtrick@gmail.com](madtrick@gmail.com)
  
If you like the project, please star it :smile:
  
## License <a name="license"></a>
Copyright [2014] [Farruco Sanjurjo Arcay]

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

