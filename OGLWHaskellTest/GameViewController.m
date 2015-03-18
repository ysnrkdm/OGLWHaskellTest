//
//  GameViewController.m
//  OGLTest
//
//  Created by Kodama Yoshinori on 2/11/15.
//  Copyright (c) 2015 EuphonicTeck. All rights reserved.
//

#import "GameViewController.h"
#import <OpenGLES/ES2/glext.h>
#import "Main_stub.h"

@interface GameViewController () {
    float ox, oy;
    float x, y;
}
@property (strong, nonatomic) EAGLContext *context;

- (void)setupGL;
- (void)tearDownGL;
@end

@implementation GameViewController

- (void)viewDidLoad
{
    [super viewDidLoad];

    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];

    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }

    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    view.drawableDepthFormat = GLKViewDrawableDepthFormat24;

    [self setupGL];
}

- (void)dealloc
{
    [self tearDownGL];

    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];

    if ([self isViewLoaded] && ([[self view] window] == nil)) {
        self.view = nil;

        [self tearDownGL];

        if ([EAGLContext currentContext] == self.context) {
            [EAGLContext setCurrentContext:nil];
        }
        self.context = nil;
    }

    // Dispose of any resources that can be recreated.
}

- (BOOL)prefersStatusBarHidden {
    return YES;
}

- (void)setupGL
{
    [EAGLContext setCurrentContext:self.context];

    loadShaders();

    glEnable(GL_DEPTH_TEST);
}

- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
}

#pragma mark - Touches

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    CGPoint loc = [self getPositionFromTouch:[touches anyObject]];
    ox = loc.x;
    oy = loc.y;
    x = loc.x;
    y = loc.y;
    touchesBegan(loc.x, loc.y);
}

- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    CGPoint loc = [self getPositionFromTouch:[touches anyObject]];
    x = loc.x;
    y = loc.y;
    touchesMoved(loc.x, loc.y, ox, oy);
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    CGPoint loc = [self getPositionFromTouch:[touches anyObject]];
    x = 0;
    y = 0;
    touchesEnded(loc.x, loc.y, ox, oy);
}

- (CGPoint)getPositionFromTouch:(UITouch *)touch
{
    return [touch locationInView:self.view];
}

#pragma mark - GLKView and GLKViewController delegate methods

- (void)update
{
}

- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    extern void (*drawFrame)(double, double, double, double, double, double, double);
    drawFrame(self.timeSinceLastUpdate, self.view.bounds.size.width, self.view.bounds.size.height, x, y, ox, oy);
}

@end
