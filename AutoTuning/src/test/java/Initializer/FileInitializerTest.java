package Initializer;

import org.junit.Assert;
import org.junit.Test;

public class FileInitializerTest {

    @Test(expected = RuntimeException.class)
    public void testEmptyPath() {

        // Given
        String path = "";

        // When
        FileInitializer fi = new FileInitializer(path);
    }

    @Test
    public void testNotExistingFile() {

        // Given
        String path = "test";

        // When
        FileInitializer fi = new FileInitializer(path);
        fi.setExtension();
        String extension = fi.getExtension();

        // Then
        Assert.assertFalse(extension == null);
    }

    @Test
    public void testExcelFile() {

        // Given
        String path = "src/test/resources/train.csv";

        // When
        FileInitializer fi = new FileInitializer(path);
        fi.setExtension();
        String extension = fi.getExtension();

        // Then
        Assert.assertTrue(extension.equals("csv"));

    }

}