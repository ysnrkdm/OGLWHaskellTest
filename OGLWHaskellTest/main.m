//
//  main.m
//  FB
//
//  Created by Kodama Yoshinori on 2/9/15.
//  Copyright (c) 2015 EuphonicTeck. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "AppDelegate.h"

void (*drawFrame)(double, double, double, double, double, double, double);

void c_main(void (*_drawFrame)(double, double, double, double, double, double, double)) {
    NSLog(@"%@", [[NSBundle mainBundle] pathForResource:@"Shader" ofType:@"vsh"]);
    int argc_ = 1;
    char* argv_[2];
    printf("c_main\n");
    drawFrame = _drawFrame;
    printf("c_main continues...\n");
    argv_[0] = "dummy";
    argv_[1] = "";
    @autoreleasepool {
        UIApplicationMain(argc_, argv_, nil, NSStringFromClass([AppDelegate class]));
    }
}

const char *c_pathForResource(char *path, char *ofType) {
    NSString* pathStr = [NSString stringWithUTF8String:path];
    NSString* ofTypeStr = [NSString stringWithUTF8String:ofType];
    NSLog(@"Finding bundle path for %@, %@\n", pathStr, ofTypeStr);
    const char *ret = [[[NSBundle mainBundle] pathForResource:pathStr ofType:ofTypeStr] UTF8String];
    NSLog(@"Found : %s\n", ret);
    return ret;
}