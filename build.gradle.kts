plugins {
    kotlin("jvm") version "1.3.61"
}

group = "funlang"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    jcenter()
    maven("https://dl.bintray.com/jannis/kotlin-pretty")
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
    implementation("kotlin-pretty:kotlin-pretty:0.5.2")
    implementation("io.arrow-kt:arrow-core:0.10.3")
    testImplementation("org.junit.jupiter:junit-jupiter:5.5.2")
    implementation("org.jetbrains.kotlinx:kotlinx-collections-immutable-jvm:0.3.2")
}

tasks {
    compileKotlin {
        kotlinOptions.jvmTarget = "1.8"
        kotlinOptions.freeCompilerArgs = listOf("-XXLanguage:+InlineClasses")
    }
    compileTestKotlin {
        kotlinOptions.jvmTarget = "1.8"
        kotlinOptions.freeCompilerArgs = listOf("-XXLanguage:+InlineClasses")
    }

    test {
        useJUnitPlatform {
        }
        testLogging {
            events("passed", "skipped", "failed")
        }
    }
}
