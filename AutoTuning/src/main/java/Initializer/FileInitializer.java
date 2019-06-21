package Initializer;

import org.apache.commons.io.FilenameUtils;

public class FileInitializer {

    private String path;
    private String extension;

    public FileInitializer(String path) {
        if (path == "") {
            throw new RuntimeException("Path cannot be empty.");
        }
        this.path = path;
    }

    public void setExtension() {
        this.extension = FilenameUtils.getExtension(this.path);
    }

    public String getExtension() {
        return extension;
    }

}
