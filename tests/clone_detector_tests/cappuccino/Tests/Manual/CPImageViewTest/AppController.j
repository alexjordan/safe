/*
 * AppController.j
 * CPImageViewTest
 *
 * Created by Alexander Ljungberg on March 9, 2012.
 * Copyright 2012, SlevenBits Ltd All rights reserved.
 */

@import <Foundation/CPObject.j>


@implementation AppController : CPObject
{
}

- (void)applicationDidFinishLaunching:(CPNotification)aNotification
{
    var theWindow = [[CPWindow alloc] initWithContentRect:CGRectMakeZero() styleMask:CPBorderlessBridgeWindowMask],
        contentView = [theWindow contentView];

    var label = [[CPTextField alloc] initWithFrame:CGRectMakeZero()];

    [label setStringValue:@"The second image should be the left image with a shadow."];

    [label sizeToFit];

    [label setFrameOrigin:CGPointMake(20.0, 20.0)];

    [contentView addSubview:label];

    var imageView = [[CPImageView alloc] initWithFrame:CGRectMake(20.0, 50.0, 290.0, 250.0)],
        image = CPImageInBundle("280px-1730_Homann_Map_of_Scandinavia,_Norway,_Sweden,_Denmark,_Finland_and_the_Baltics_-_Geographicus_-_Scandinavia-homann-1730.jpg", CGSizeMake(280, 240)),
        imageViewShadow = [[CPImageView alloc] initWithFrame:CGRectMake(320.0, 50.0, 290.0, 250.0)];

    [imageView setImageScaling:CPImageScaleNone];
    [imageViewShadow setImageScaling:CPImageScaleNone];

    [imageView setImage:image];
    [imageViewShadow setImage:image];
    [imageViewShadow setHasShadow:YES];

    [contentView addSubview:imageView];
    [contentView addSubview:imageViewShadow];

    [theWindow orderFront:self];

    // Uncomment the following line to turn on the standard menu bar.
    //[CPMenu setMenuBarVisible:YES];
}

@end
