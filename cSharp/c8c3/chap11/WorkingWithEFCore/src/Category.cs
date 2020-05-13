using System.Collections.Generic;
using System.ComponentModel.DataAnnotations.Schema;

namespace Packt.Shared
{
    public class Category
    {
        // Define properties map to columns in the DB
        public int CategoryID { get; set; }

        public string CategoryName { get; set; }

        [Column(TypeName = "ntext")]
        public string Description { get; set; }
        
        // Define a navigation property for related rows
        public virtual ICollection<Product> Products { get; set; }

        public Category()
        {
            // The navigation property must be initialized to an empty list
            // so developers can add product to a Category
            this.Products = new List<Product>();
        }
    }
}