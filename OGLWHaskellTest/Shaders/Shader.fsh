//
//  Shader.fsh
//  OGLWHaskellTest
//
//  Created by Kodama Yoshinori on 3/1/15.
//  Copyright (c) 2015 EuphonicTeck. All rights reserved.
//

varying lowp vec4 colorVarying;

void main()
{
    gl_FragColor = colorVarying;
}
