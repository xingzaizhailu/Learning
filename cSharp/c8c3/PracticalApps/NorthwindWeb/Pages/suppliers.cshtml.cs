using System.Linq;
using Packt.Shared;
using System.Collections.Generic;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;

namespace NorthwindWeb.Pages
{
    public class SuppliersModel : PageModel
    {
        public IEnumerable<string> Suppliers { get; set; }
        private Northwind db;
        [BindProperty]  // connect HTML elements on the web page to properties in the Supplier Class
        public Supplier Supplier { get; set; }

        public SuppliersModel(Northwind injectedContext)
        {
            db = injectedContext;
        }

        public void OnGet()
        {
            ViewData["Title"] = "Northwind Web Site - Suppliers";

            Suppliers = db.Suppliers.Select(s => s.CompanyName);
        }

        public IActionResult OnPost()
        {
            if (ModelState.IsValid) // check all property values conform to validation rules
            {
                db.Suppliers.Add(Supplier);
                db.SaveChanges();
                return RedirectToPage("/suppliers");
            }
            return Page();
        }
    }
}