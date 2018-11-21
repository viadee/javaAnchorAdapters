package de.viadee.anchorj.image;

import java.awt.image.BufferedImage;
import java.io.Serializable;

/**
 * This interface is used to describe images.
 * <p>
 * Depending on the use case, certain image manipulation operations may be expensive. Therefore, different
 * implementations/libraries may be faster than others.
 * <p>
 * Hence, this interface shall enable the use of different, optimized image implementations.
 */
public interface ImageRepresentation extends Serializable {

    /**
     * Returns a blank image of the same size as the current one.
     *
     * @return a blank {@link de.viadee.anchorj.image.ImageRepresentation}
     */
    de.viadee.anchorj.image.ImageRepresentation createBlankCanvas();

    /**
     * @return a {@link BufferedImage} with the same content as.
     */
    BufferedImage asBufferedImage();

    /**
     * Used for image manipulation and to pass the content to prediction models.
     *
     * @return a byte array of the image content.
     */
    byte[] asByteArray();

    /**
     * @return the height of the image
     */
    int getHeight();

    /**
     * @return the width of the image
     */
    int getWidth();

    /**
     * @param x x-coordinate
     * @param y y-coordinate
     * @return the pixel value at (x,y)
     */
    int getPixel(int x, int y);

    /**
     * @param x   x-coordinate
     * @param y   y-coordinate
     * @param rgb the value to set at (x,y)
     */
    void setPixel(int x, int y, int rgb);
}
