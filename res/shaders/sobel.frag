#version 450 core

#define MAX_POINT_LIGHTS 4

layout (row_major, std140) uniform;

in VS_OUT {
  vec2 uv;
} fs_in;

layout (binding = 0) uniform sampler2D screen_texture;

out vec4 color;

mat3 sx = mat3(1.0, 2.0, 1.0,
               0.0, 0.0, 0.0,
               -1.0, -2.0, -1.0
               );

mat3 sy = mat3(1.0, 0.0, -1.0,
               2.0, 0.0, -2.0,
               1.0, 0.0, -1.0
               );

void main() {
  vec3 diffuse = texture(screen_texture, uv).rgb;
  mat3 I;
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      vec3 sample = texelFetch(screen_texture,
                               ivec2(gl_FragCoord) + ivec2(i-1, j-1),
                               0).rgb;
      I[i][j] = length(sample);
    }
  }

  float gx = dot(sx[0], I[0]) + dot(sx[1], I[1]) + dot(sx[2], I[2]);
  float gy = dot(sy[0], I[0]) + dot(sy[1], I[1]) + dot(sy[2], I[2]);

  float g = sqrt(pow(gx, 2) + pow(gy, 2));
  color = vec4(diffuse - vec3(g), 1.0);
}
