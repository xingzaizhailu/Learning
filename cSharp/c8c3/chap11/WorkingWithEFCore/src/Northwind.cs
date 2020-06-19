using Microsoft.EntityFrameworkCore;

namespace Packt.Shared
{
    // Manage the connection to the DB
    public class Northwind : DbContext
    {
        // Properties map to tables in the DB
        public DbSet<Category> Categories { get; set; }
        public DbSet<Product> Products { get; set; }

        protected override void OnConfiguring(
            DbContextOptionsBuilder optionsBuilder)
        {
            string path = System.IO.Path.Combine(
                System.Environment.CurrentDirectory, "Northwind.db");

            optionsBuilder.UseSqlite($"Filename={path}");
        }

        protected override void OnModelCreating(
            ModelBuilder modelBuilder)
        {
            // eg. of using Fluent API instead of attributes
            // to limit the length of a category name to 15
            modelBuilder.Entity<Category>()
                .Property(category => category.CategoryName)
                .IsRequired()
                .HasMaxLength(15);
        }
    }
}