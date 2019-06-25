## Copy Constructors
A special constructor that duplicates an existing object.  

- must take a reference to an existing object of the same type  
- makes an identical copy of the existing object  

### Shallow copy and deep copy
- Shallow copy - only duplicate immediate fields, not the contents of any
sub-classes  
- Deep copy - duplicate all contents of every field  

``` cpp
    // example
    class VideoCharacter {
    public:
        VideoCharacter(std::string& name);
        VideoCharacter(VideoCharacter& other);
        ~VidelCharacter();      // Deconstructor

    private:
        std::string name;
        int         hp;
    }
```

## Initialising Fields in Constructors with Colon (:)
```
    VideoCharacter::VideoCharacter(std::string& name) :
        name(name),
        hp(MAX_HP)
    {}

    VideoCharacter::VideoCharacter(VideoCharacter& other) :
        name(other.name),
        hp(other.hp)
    {}
```

## Object Ownership
The concept of who has responsibility for managing the life-cycle of memory
allocated on the heap (programmer-managed memory). Including two aspects:  
- who controls which other code can modify a piece of memory  
- who must de-allocate memory when it is no longer required  

### Ownership transferring
