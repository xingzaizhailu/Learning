## Chap 16: Building Websites Using the Model-View-Controller Pattern

### Setting up an ASP.NET Core MVC website

- **Models:** Classes that represent the data entities and view models used in the website
- **View:** Razor files (`.cshtml`) that render data in view models into HTML web pages. Blazor uses the `.razor` file extension, but do not confuse them with Razor files.
- **Controllers:** Classes that execute code when an HTTP request arrives at the web server. The code usually creates a view model that may contain entity models and passes it to a view to generate an HTTP response to send back to the web browser or other client.

```shell
$ dotnet new mvc --auth Individual
```

It will generate a website supports register and login.

- `Areas`: Contains files needed for features like ASP.NET Core Identity for authentication
- `bin`, `obj`: Contain the compiled assemblies
- `Controllers`: Contains C# classes that have methods (known as actions) that fetch a _model_ and pass it to a _view_.
- `Properties`: Contains a configuration file for IIS and launching the website during development named `launchSettings.json`
- `Views`: Contains the `.cshtml` Razor files that combine HTML and C# code to dynamically generate HTML responses. The `_ViewStart` file sets the default layout and the `_ViewImports` imports common namespaces used in all views like tag helpers.
  - Home: This subfolder contains Razor files for the home and privacy pages.
  - Shared: Contains Razor files for the shared layout, an error page, and some partial views for logging in, accepting privacy policy, and managing the consent cookie.
- `wwwroot`: This folder contains static content used in the website, such as CSS for styling, images, JavaScript and a `favicon.ico` file.
- `app.db`: The SQLite database that stores registered visitors.
- `appsettings.json` and `appsettings.Development.json`: contain settings that your website can load at runtime, for example, the database connection string for the ASP.NET Identity system and logging levels.
- `NorthwindMVC.csproj`: Contains project settings like use of the Web .NET SDK, an entry to ensure the `app.db` file is copied to the website's output folder, and a list of NuGet packages the project requires.
- `Program.cs`: Defines a class that contains the `Main` entry that builds a pipeline for processing incoming HTTP requests and host a website using default options like configuring the Kestrel web server and loading `appsettings`. While building the host it calls the `UseStartup<T>()` method to specify another class that performs additional configuration
- `Startup.cs`: This file adds and configures services that your website needs. For example, ASP.NET Identity for authentication, SQLite for data storage and so on, and routes for your app.

### Exploring an ASP.NET Core MVC website

#### Understanding ASP.NET Core MVC startup

#### Understanding the default MVC route

#### Understanding controllers and actions

The responsibility of a controller:

- Identify the services that the controller needs in order to be in a valid state and to function properly in their class constructor(s).
- Use the action name to identify a method to execute.
- Extract parameters from the HTTP request.
- Use the parameters to fetch any additional data needed to construct a view model and pass it to the appropriate view for the client.
  - For example, if the client is a web browser, then a view that renders HTML would be most appropriate. Other clients might prefer alternative renderings like document formats such as a PDF file or an Excel file, or data formats like JSON or XML.
- Return the results from the view to the client as an HTTP response with an appropriate status code.

#### Understanding filters

When you need to add some functionality to multiple controllers and actions then you can use or define your own filters that are implemented as an attribute class.

Filters can be applied at the following levels:

- Action-level: by decorating the method
- Controller-level: by decorating the class
- Global-level: by adding an instance of the attribute to the `Filters` collection of the `IServiceCollection` in the `ConfigureServices` method of `Startup` class. This will affect all methods of all controllers in the project.

##### Using a filter to secure an action method

```csharp
// Ensure this method can only be called by members of certain security roles
[Authorize(Roles = "Sales,Marketing")]
public IActionResult SalesAndMarketingEmployeesOnly()
{
	return View();
}
```

##### Using a filter to cache a response

```
[ResponseCache(Duration = 3600,  	
 Location = ResponseCacheLocation.Any)]
// No
public IActionResult AboutUs()
{
	return View();
}
```

- `Duration`: In seconds. This sets the 'max-age' HTTP response header.
- `Location`: Any, Client, or None. This sets the cache-control HTTP response header.
- `NoStore`: If true, this ignores `Duration` and `Location` and sets the `cache-control` HTTP response header to `no-store`.

##### Using a filter to define a custom route

For URL `https://localhost:5001/home/privacy`, we could decorate the action method to make the route simpler:

```csharp
[Route("private")]
public IActionResult Privacy()
{
	return View();
}
```

Now, we can use `https://localhost:5001/private`.

#### Understanding entity and view models

- Entity models: entities in a data store like SQLite
- View Models: data we want to show in response. It is passed into a _view_ for rendering into a response format like HTML or JSON.

When the `View()` method is called in a controller's action method, ASP.NET Core MVC looks in the Views folder for a subfolder with the same name as the current controller. It then looks for a file with the same name as the current action, that is, `{Action}.cshtml`.

#### Understanding views

The responsibility of a view is to transform a model into HTML or other formats.

There are multiple **view engines**. The default view engine is **Razor**, and it uses `@` symbol to indicate server-side code execution.

### Customising an ASP.NET Core MVC website

