## Redux
### Basic Concepts and API
#### Store
Store 就是保存数据的地方，你可以把它看成一个容器。整个应用只能有一个 Store。
Store对象包含所有数据。如果想得到某个时点的数据，就要对 Store 生成快照。这种时点的数据集合，就叫做
State。
当前时刻的 State，可以通过store.getState()拿到。
