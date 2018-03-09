#version 450 core

#define MAX_POINT_LIGHTS 4

layout (row_major, std140) uniform;

in VS_OUT {
  vec3 pos;
  vec3 norm;
  vec3 view;
  vec2 uv;
  vec3[MAX_POINT_LIGHTS] light;
  float[MAX_POINT_LIGHTS] intensity;
} fs_in;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (binding = 0) uniform Camera {
  mat4 mvp;
  mat4 mv;
  mat4 p;
} camera;

layout (binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (binding = 2) uniform Material {
  vec4 diffuse_color;
  vec4 ambient_color;
  vec4 specular_color;
  float specular_strength;
  float specular_exponent;
} material;

layout (binding = 0) uniform sampler2D tex_color;
layout (binding = 5) uniform sampler2D tex_normal;
layout (binding = 11) uniform sampler1D tex_specular;

out vec4 color;

void main() {
  vec3 N = normalize(fs_in.norm);
  vec3 V = normalize(fs_in.view);

  vec3 diffuse = vec3(0);
  vec3 specular = vec3(0);
  float specular_index;
  int i;
  for (i = 0; i < min(MAX_POINT_LIGHTS, point_lights.num); i++) {
    vec3 L = normalize(fs_in.light[i]);
    vec3 R = reflect(-L, N);

    diffuse += max(dot(N, L), 0.0) * material.diffuse_color.rgb * fs_in.intensity[i];
    specular_index = pow(max(dot(R, V), 0.0), material.specular_exponent) *
      material.specular_strength *
      fs_in.intensity[i];
    specular += -texture(tex_specular, specular_index).rgb + 1;
  }
  vec3 outline = vec3(0);
  // high poly smoother
  float outliner = abs(dot(V, N));
  if (outliner < 0.1) {
    outline = mix(vec3(0), vec3(-1), outliner / 0.1);
  }

  vec3 tex = texture(tex_color, floor(5 * fs_in.uv) / 5).rgb;
  color = vec4(diffuse * 0.1 + specular * 0.1 + tex + outline + material.ambient_color.rgb * 0.1, 1.0);
}
