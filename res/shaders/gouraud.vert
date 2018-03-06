#version 450 core

#define MAX_POINT_LIGHTS 128

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

layout (std140, binding = 0) uniform Camera {
  layout (offset = 0) mat4 mvp;
  layout (offset = 64) mat4 mv;
  layout (offset = 128) mat4 p;
} camera;

struct PointLight {
  vec4 position;
  float intensity;
};

layout (std140, binding = 1) uniform PointLights {
  PointLight[MAX_POINT_LIGHTS] lights;
  int num;
} point_lights;

layout (std140, binding = 2, align = 16) uniform Material {
  layout (offset = 0) vec4 diffuse_color;
  layout (offset = 16) vec4 ambient_color;
  layout (offset = 32) vec4 specular_color;
  layout (offset = 48) float specular_strength;
  layout (offset = 52) float specular_exponent;
} material;

out VS_OUT {
  vec3 lighting;
  vec2 uv;
} vs_out;

void main() {
  vec4 pos = vec4(position, 1) * camera.mv;

  vec3 view_vector = normalize(- pos.xyz);

  // Calculate the normal in view space.
  vec3 norm = normalize(normal * mat3(camera.mv));

  vec3 diffuse = {0, 0, 0};
  vec3 specular = {0, 0, 0};
  int i;
  for (i = 0; i < min(point_lights.num, MAX_POINT_LIGHTS); i++) {
    vec3 light_vector_normed = normalize(point_lights.lights[i].position.xyz - pos.xyz);
    vec3 light_vector = point_lights.lights[i].intensity * light_vector_normed;
    vec3 reflect_vector = reflect(-light_vector, norm);

    diffuse += max(dot(norm, light_vector), 0.0) * material.diffuse_color.rgb;
        specular += pow(max(dot(reflect_vector, view_vector), 0.0),
                  material.specular_exponent) *
          material.specular_strength * material.specular_color.rgb;

  }

  vs_out.lighting = material.ambient_color.rgb + diffuse + specular;
  vs_out.uv = uv;
  gl_Position = pos * camera.p;
}
