using System.Collections.Generic;
using Packt.Shared;

namespace NorthwindMvc.Models
{
    // A good naming practice for view model classes:
    // {Controller}{Action}ViewModel 
    public class HomeIndexViewModel
    {
        public int VisitorCount;
        public IList<Category> Categories { get; set;}
        public IList<Product> Products { get; set;}
    }
}