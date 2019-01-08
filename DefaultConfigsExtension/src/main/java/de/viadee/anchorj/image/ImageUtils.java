package de.viadee.anchorj.image;

import org.imgscalr.Scalr;

import java.awt.image.BufferedImage;

/**
 * Offers utility methods to manipulate images.
 */
public enum ImageUtils {;

    /**
     * Resize and crop a {@link BufferedImage}.
     *
     * @param image  the image
     * @param width  the width
     * @param height the height
     * @return the buffered image
     */
    public static BufferedImage resizeAndCrop(BufferedImage image, int width, int height) {
        image = Scalr.resize(image, Scalr.Method.ULTRA_QUALITY,
                image.getHeight() < image.getWidth() ? Scalr.Mode.FIT_TO_HEIGHT : Scalr.Mode.FIT_TO_WIDTH,
                Math.max(width, height), Math.max(width, height), Scalr.OP_ANTIALIAS);
        return Scalr.crop(image, width, height);
    }
}
