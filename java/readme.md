Extism Java-SDK
---

Java SDK for the [extism](https://extism.org/) WebAssembly Plugin-System.

# Build

To build the extism java-sdk run the following command:

```
mvn clean verify
```

# Usage

To use the extism java-sdk you need to add the `org.extism.sdk` dependency to your dependency management and ensure that  
the native extism library is installed on your system. For installing the native library refer to the [extism documentation](https://extism.org/docs/install).

Instead of installing the native library on your system, you can also download the appropriate library for your platform
yourself.  
To do that simply download the [extism release](https://github.com/extism/extism/releases) to a `folder` and  run your java application with the system property `-Djna.library.path=/path/to/folder`.

## Maven
To use the extism java-sdk with maven you need to add the following dependency to your `pom.xml` file:
```xml
<dependency>
    <groupId>org.extism.sdk</groupId>
    <artifactId>extism</artifactId>
    <version>0.1.0</version>
</dependency>
```

## Gradle
To use the extism java-sdk with maven you need to add the following dependency to your `build.gradle` file:

```
implementation 'org.extism.sdk:extism:0.1.0'
```

