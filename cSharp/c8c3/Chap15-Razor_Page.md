## Chap 15: Building Websites Using ASP.NET Core Razor Pages

### Commands

```shell
$ dotnet new web
$ dotnet build
$ dotnet run
$ dotnet dev-certs https --trust. // Trusting HTTPS development certificate was requested
```



```csharp
if (env.IsDevelopment()) {
	app.UseDeveloperExceptionPage(); }
else
{
	app.UseHsts();		// HTTP Strict Transport Security (HSTS), forces HTTPS
}
...
app.UseHttpsRedirection();  // after app.UseRouting, to redirect HTTP reqs to HTTPS
```

### Enabling static and default files

Create a `index.html` file under `wwwroot` folder.

```csharp
app.UseDefaultFiles(); // index.html, default.html, and so on app.UseStaticFiles();
app.UseEndpoints(endpoints =>
{
  // endpoints.MapGet("/", async context =>
  // {
  // 		await context.Response.WriteAsync("Hello World!");
  // });
});
```

### Exploring Razor Pages

Allows mix HTML markup with C# code.

By default, ASP.NET Core looks for Razor Pages in `Pages` folder. Move `index.html` into `Pages`, rename `.html` to `.cshtml`.

```csharp
services.AddRazorPages();  		// add to Startup.ConfigureServices

endpoints.MapRazorPages();    // add to
```

#### Defining a Razor Page

```csharp
@page
@functions
{
	public string DayName { get; set; }
	
  public void OnGet()
  {
		Model.DayName = DateTime.Now.ToString("dddd");
  }
}

<p>It's @Model.DayName! Our customers include restaurants, hotels, and cruise lines.</p>
```

#### Using shared layouts with Razor Pages

The name of this file can be anything, but `_Layout.cshtml` is a good practice. We must also create a specially named file (`_ViewStart.cshtml`) to set the default layout for all Razor Pages (and all MVC views).

`_ViewStart.cshtml` in `Pages`:

```
@{
	Layout = "_Layout";
}
```

In `Pages`, create a folder named `Shared`. In `Shared` create a file named `_Layout.cshtml`.

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <!-- Required meta tags -->
  	<meta charset="utf-8" />
    <meta name="viewport" content=
        "width=device-width, initial-scale=1, shrink-to-fit=no" />
    <!-- Bootstrap CSS -->
    <link rel="stylesheet"
          href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
          integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
          crossorigin="anonymous">
    <title>@ViewData["Title"]</title>
</head>

<body>
    <div class="container">
      	@RenderBody() <!-- the insertion point for the page being requested. -->
      	<ht />
      	<footer>
						<p>Copyright &copy; 2020 - @ViewData["Title"]</p>
      	</footer>
		</div>
		<!-- JavaScript to enable features like carousel -->
		<!-- jQuery first, then Popper.js, then Bootstrap JS -->
  	<script
    		src="https://code.jquery.com/jquery-3.3.1.slim.min.js"
				integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRv H+8abtTE1Pi6jizo"
        crossorigin="anonymous">
  	</script>
		<script
    		src="https://cdnjs.cloudflare.com/ajax/libs/ popper.js/1.14.7/umd/popper.min.js"
        integrity="sha384-UO2eT0 CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
        crossorigin="anonymous">
  	</script>
		<script
    		src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/ js/bootstrap.min.js"
        integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoII y6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
        crossorigin="anonymous">
    </script>
  	<!-- a Razor Page can optionally inject additional scripts that it needs. -->
		@RenderSection("Scripts", required: false)
</body>
</html>
```

Modify `index.html`:

```html
@page
@functions
{
  public string DayName { get; set; }

  public void OnGet()
  {
    ViewData["Title"] = "Northwind Website";
    Model.DayName = DateTime.Now.ToString("dddd");
  }
}

<div class="jumbotron">
  <h1 class="display-3">Welcome to Northwind!</h1>
  <p class="lead">We supply products to our customers.</p>
  <hr />
  <p>It's @Model.DayName! Our customers include restaurants, hotels and cruise lines.</p>
  <p>
    <a class="btn btn-primary" href="suppliers">Learn more about suppliers</a>
  </p>
</div>
```

#### Using code-behind files with Razor Pages

Sometimes, it's better to separate the HTML markup from the data and executable code, so Razor allows **code-behind** class files.

Create a `suppliers.cshtml.cs` under `Pages`:

```csharp
using Microsoft.AspNetCore.Mvc.RazorPages;
using System.Collections.Generic;

namespace NorthwindWeb.Pages
{
	public class SuppliersModel : PageModel
  {
		public IEnumerable<string> Suppliers { get; set; }
    
    public void OnGet()
    {
			ViewData["Title"] = "Northwind Web Site - Suppliers";
			Suppliers = new[] {
				"Alpha Co", "Beta Limited", "Gamma Corp"
			};
    }
	}
}
```

 `SuppliersModel` is inherit from `PageModel`, so it has members such as the `ViewData` dict for sharing data.

In `suppliers.cshtml`:

```html
@page
<!-- Model type for this Razor page is set to SuppliersModel -->
@model NorthwindWeb.Pages.SuppliersModel

<div class="row">
  <h1 class="display-2">Suppliers</h1>
  <table class="table">
    <thead class="thead-inverse">
      <tr><th>Company Name</th></tr>
    </thead>
    <tbody>
      <!-- iter through the Suppliers property of the Model -->
      @foreach(string name in Model.Suppliers)
      {
          <tr><td>@name</td></tr>
      }
    </tbody>
  </table>
</div>
```

### Using Entity Framework Core with ASP.NET Core

### Manipulating data using Razor pages

#### Enabling a model to insert entities

### Using Razor class libraries

### Using a Razor class library