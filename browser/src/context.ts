import { Manifest, PluginConfig, ManifestWasmFile, ManifestWasmData } from './manifest';
import { ExtismPlugin } from './plugin';

/**
 * Can be a {@link Manifest} or just the raw bytes of the WASM module as an ArrayBuffer.
 * We recommend using {@link Manifest}
 */
type ManifestData = Manifest | ArrayBuffer;

/**
 * A Context is needed to create plugins. The Context
 * is where your plugins live. Freeing the context
 * frees all of the plugins in its scope.
 */
export default class ExtismContext {
  /**
   * Create a plugin managed by this context
   *
   * @param manifest - The {@link ManifestData} describing the plugin code and config
   * @param config - Config details for the plugin
   * @returns A new Plugin scoped to this Context
   */
  async newPlugin(manifest: ManifestData, functions: Record<string, any> = {}, config?: PluginConfig) {
    let moduleData: ArrayBuffer | null = null;
    if (manifest instanceof ArrayBuffer) {
      moduleData = manifest;
    } else if ((manifest as Manifest).wasm) {
      const wasmData = (manifest as Manifest).wasm;
      if (wasmData.length > 1) throw Error('This runtime only supports one module in Manifest.wasm');
      const wasm = wasmData[0];
      if ((wasm as ManifestWasmData).data) {
        moduleData = (wasm as ManifestWasmData).data;
      } else if ((wasm as ManifestWasmFile).path) {
        const response = await fetch((wasm as ManifestWasmFile).path);
        moduleData = await response.arrayBuffer();
        console.dir(moduleData);
      }
    }
    if (!moduleData) {
      throw Error(`Unsure how to interpret manifest ${manifest}`);
    }

    return new ExtismPlugin(moduleData, functions, config);
  }
}
